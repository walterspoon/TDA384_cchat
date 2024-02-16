-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server, % atom of the chat server
    channels % list of channels the client is currently in
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom,
        channels = []  %% Initialize channels to an empty list
    }.


% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client
        
% Join channel
handle(St, {join, Channel}) ->
    case lists:member(St#client_st.server, registered()) of
    true -> 
        case lists:member(Channel, St#client_st.channels) of
            true -> {reply, {error, already_joined, "Already joined this channel"}, St};
            false -> 
                Result = (catch genserver:request(St#client_st.server, {join, Channel, self()})),
                case Result of
                    {'EXIT',_} -> {reply, {error, server_not_reached, "Server does not respond"}, St};
                    ok -> {reply, ok, St#client_st{channels = [Channel | St#client_st.channels]}};
                    failed -> {reply, {error, user_already_joined, "Already in channel"}, St}
                end
        end;
    false -> {reply, {error, server_not_reached, "Server unreachable"}, St}
  end;


% Leave channel
handle(St, {leave, Channel}) ->
    case lists:member(Channel, St#client_st.channels) of
        true -> 
            Result = (catch genserver:request(St#client_st.server, {leave, Channel, self()})),
            case Result of
                {'EXIT',_} -> {reply, {error, server_not_reached, "Server does not respond"}, St};
                ok -> {reply, ok, St#client_st{channels = lists:delete(Channel, St#client_st.channels)}}
            end;
        false -> {reply, {error, not_in_channel, "You are not in this channel"}, St}
    end;

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    case lists:member(Channel, St#client_st.channels) of
        true -> 
            Result = (catch genserver:request(St#client_st.server, {message_send, Channel, St#client_st.nick, Msg})),
            case Result of
                {'EXIT',_} -> {reply, {error, server_not_reached, "Server does not respond"}, St};
                ok -> {reply, ok, St};
                failed -> {reply, {error, message_not_sent, "Message not sent"}, St}
            end;
        false -> {reply, {error, not_in_channel, "You are not in this channel"}, St}
    end;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    {reply, ok, St#client_st{nick = NewNick}} ;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, Data) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St}.
