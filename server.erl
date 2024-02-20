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

start(ServerAtom) ->
    genserver:start(ServerAtom, initial_state(), fun handle_call/2).

handle_call(State, {join, From, Nick, Channel}) ->
    io:fwrite("test!"),
    %Channels = State#state.channels,
    case lists:member(Channel, State#state.channels) of
        true ->
            % The channel exists
            Result = genserver:request(list_to_atom(Channel), {join, From}),
			{reply, Result, State};
        false ->
            % The channel does not exist
            NewNicks = [Nick | State#state.nicks],
            NewChannels = [Channel | State#state.channels],
            NewState = State#state{channels = NewChannels, nicks = NewNicks},
            channel:start(Channel),
            io:fwrite("Channel ~p created!~n", [Channel]),
            {reply, ok, NewState}
    end;

handle_call(State, _) ->
    {reply, ok, State}.

stop(ServerAtom) ->
    genserver:stop(ServerAtom),
    ok.