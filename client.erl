-module(client).
-export([handle/2, initial_state/3]).

% This record defines the structure of the state of a client.
% Add whatever other fields you need.
-record(client_st, {
    gui, % atom of the GUI process
    nick, % nick/username of the client
    server % atom of the chat server
}).

% Return an initial state record. This is called from GUI.
% Do not change the signature of this function.
initial_state(Nick, GUIAtom, ServerAtom) ->
    #client_st{
        gui = GUIAtom,
        nick = Nick,
        server = ServerAtom
    }.

% Send request via genserver
sendRequest(RecieverPid, Request) ->
    try genserver:request(RecieverPid, Request) of
        Response -> Response
    catch
        timeout_error -> {error, server_not_reached, "Server not reached!"}
    end.

% handle/2 handles each kind of request from GUI
% Parameters:
%   - the current state of the client (St)
%   - request data from GUI
% Must return a tuple {reply, Data, NewState}, where:
%   - Data is what is sent to GUI, either the atom `ok` or a tuple {error, Atom, "Error message"}
%   - NewState is the updated state of the client
        
% Join channel
handle(St, {join, Channel}) ->
    Server = St#client_st.server,
    Result = sendRequest(Server, {join, self(), St#client_st.nick, Channel}),
    {reply, Result, St};

% Leave channel
handle(St, {leave, Channel}) ->
    % Result = Channel ! {St#client_st.nick, {leave, self()}},
    Result = sendRequest(list_to_atom(Channel), {leave, self()}),
    {reply, Result, St};

% Sending message (from GUI, to channel)
handle(St, {message_send, Channel, Msg}) ->
    case whereis(list_to_atom(Channel)) of
        undefined ->
            {reply, {error, server_not_reached, "Channel does not respond"}, St};
        _ ->
        Result = sendRequest(list_to_atom(Channel), {message, self(), St#client_st.nick, Msg}),
        {reply, Result, St}
    end;

% This case is only relevant for the distinction assignment!
% Change nick (no check, local only)
handle(St, {nick, NewNick}) ->
    Server = St#client_st.server,
    OldNick = St#client_st.nick,
    Result = sendRequest(Server, {nick, NewNick, OldNick}),
    io:fwrite("Result Nick Change: ~p~n", [Result]),
    case Result of
        ok ->
            io:fwrite("     NICK NOT TAKEN!     "),
            NewSt = St#client_st{nick = NewNick},
            {reply, Result, NewSt};
        {error, nick_taken, _} ->
            io:fwrite("     NICK TAKEN!     "),
            {reply, Result, St}
    end;

% ---------------------------------------------------------------------------
% The cases below do not need to be changed...
% But you should understand how they work!

% Get current nick
handle(St, whoami) ->
    {reply, St#client_st.nick, St} ;

% Incoming message (from channel, to GUI)
handle(St = #client_st{gui = GUI}, {message_receive, Channel, Nick, Msg}) ->
    io:fwrite("Vi kommer hit!  "),
    io:fwrite("Channel: ~p, Nick: ~p, Msg: ~p~n", [Channel, Nick, Msg]),
    gen_server:call(GUI, {message_receive, Channel, Nick++"> "++Msg}),
    {reply, ok, St} ;

% Quit client via GUI
handle(St, quit) ->
    % Any cleanup should happen here, but this is optional
    {reply, ok, St} ;

% Catch-all for any unhandled requests
handle(St, _) ->
    {reply, {error, not_implemented, "Client does not handle this command"}, St} .