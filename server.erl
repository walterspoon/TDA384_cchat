-module(server).
-export([start/1, stop/1, handle_call/2]).

-record(state, {
    nicks,   % list of all nicks registered so far
    channels % list of all channels created so far
}).

% Initial state
initial_state() ->
    #state{
        nicks = [],
        channels = []
    }.

% Send request via genserver
sendRequest(RecieverPid, Request) ->
    try genserver:request(RecieverPid, Request) of
        Response -> Response    % Return the response from the genserver
    catch
        timeout_error -> {error, server_not_reached, "Server not reached!"}
    end.

% Start server via genserver
start(ServerAtom) ->
    genserver:start(ServerAtom, initial_state(), fun handle_call/2).

handle_call(State, {join, From, Nick, Channel}) ->
    % Case to see if channel already exists
    case lists:member(Channel, State#state.channels) of
        true ->
            % The channel exists
            % Add the nick of joining client to list of nicks if not already in server (umerge)
            NewNicks = lists:umerge([Nick], State#state.nicks),
            Result = sendRequest(list_to_atom(Channel), {join, From}),
            NewState = State#state{nicks = NewNicks},
            {reply, Result, NewState};
        false ->
            % The channel does not exist
            % Add the nick of joining client to list of nicks if not already in server (umerge)
            NewNicks = lists:umerge([Nick], State#state.nicks),
            % Add the new channel to the list of channels
            NewChannels = [Channel | State#state.channels],
            NewState = State#state{channels = NewChannels, nicks = NewNicks},
            % Start channel
            channel:start(Channel),
            Result = sendRequest(list_to_atom(Channel), {join, From}),
            {reply, Result, NewState}
    end;

% Change nick on server
handle_call(State, {nick, Nick, OldNick}) ->
    case lists:member(Nick, State#state.nicks) of
         % If nick already exists
        true ->
            {reply, {error, nick_taken, "nick_taken"}, State};
         % Add new nick, delete old nick
        false ->
            NewNicks = lists:delete(OldNick, State#state.nicks),
            NewNickList = [Nick | NewNicks],
            NewState = State#state{nicks = NewNickList},
            {reply, ok, NewState}
    end;

% Stop all channels in server
handle_call(State, stop_channels) ->
  % Iterates through all channels registered to a server and stops them
  Channels = State#state.channels,
  lists:foreach(fun(Channel) -> channel:stop(list_to_atom(Channel)) end, Channels),
  {reply, ok, State};
            
% Catch all calls that does not pattern match
handle_call(State, _) ->
    {reply, ok, State}.

% Stop server via genserver
stop(Server) ->
    % stop all channels before stopping server
    genserver:request(Server, stop_channels),
    genserver:stop(Server),
    ok.