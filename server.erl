-module(server).
-export([start/1, stop/1, handle_call/2]).

-record(state, {
    nicks = [],   % list of all nicks registered so far
    channels = [] % list of all channels created so far
}).

initial_state() ->
    #state{
        nicks = [],
        channels = []
    }.

% Send request via genserver
sendRequest(RecieverPid, Request) ->
    try genserver:request(RecieverPid, Request) of
        Response -> Response
    catch
        timeout_error -> {error, server_not_reached, "Server not reached!"}
    end.

start(ServerAtom) ->
    genserver:start(ServerAtom, initial_state(), fun handle_call/2).

handle_call(State, {join, From, Nick, Channel}) ->
    % Case to see if channel already exists
    case lists:member(Channel, State#state.channels) of
        true ->
            % The channel exists
            NewNicks = [Nick | State#state.nicks],
            Result = sendRequest(list_to_atom(Channel), {join, From}),
            NewState = State#state{nicks = NewNicks},
            io:fwrite("Result: ~p~n", [Result]),
            {reply, Result, NewState};
        false ->
            % The channel does not exist
            NewNicks = [Nick | State#state.nicks],
            NewChannels = [Channel | State#state.channels],
            NewState = State#state{channels = NewChannels, nicks = NewNicks},
            channel:start(Channel),
            Result = sendRequest(list_to_atom(Channel), {join, From}),
            %io:fwrite("Channel ~p created!~n", [Channel]),
            %io:fwrite("Result2: ~p~n", [Result]),
            {reply, Result, NewState}
    end;

handle_call(State, {nick, Nick, OldNick}) ->
    case lists:member(Nick, State#state.nicks) of
        true ->
            {reply, {error, nick_taken, "nick_taken"}, State};
        false ->
            NewNicks = lists:delete(OldNick, State#state.nicks),
            NewNickList = [Nick | NewNicks],
            NewState = State#state{nicks = NewNickList},
            {reply, ok, NewState}
    end;

handle_call(State, kill_channels) ->
  % Iterates through all channels registered to a server and stops them
  io:fwrite("Kill channel funktionen!   "),
  Channels = State#state.channels,
  io:fwrite("Channels: ~p~n", [Channels]),
  lists:foreach(fun(Channel) -> channel:stop(list_to_atom(Channel)) end, Channels),
  {reply, ok, State};
            

handle_call(State, _) ->
    {reply, ok, State}.

stop(Server) ->
    genserver:request(Server, kill_channels),
    io:fwrite("Server ~p stopped by Walter!~n", [Server]),
    genserver:stop(Server),
    ok.