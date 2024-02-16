-module(channel).
-export([start/1, joinChannel/2, handle/2]).

-record(state, {
    nicks   % list of all nicks registered so far
}).

initial_state() ->
    #state{
        nicks = []
    }.

start(ServerAtom) ->
    genserver:start(ServerAtom, initial_state(), fun handle/2).

joinChannel(Nick, State) ->
    NewState = State#state{nicks = [Nick | State#state.nicks]},
    {ok, NewState}.

handle(State, {join, Nick}) ->
    {reply, ok, joinChannel(Nick, State)};
handle(State, {message, Sender, Msg}) ->
    BroadcastMsg = {message_receive, Sender, Msg},
    {noreply, [Client || Client <- State#state.nicks, Client /= Sender], BroadcastMsg};
handle(State, Message) ->
    io:format("Received message: ~p~n", [Message]),
    {reply, ok, State}.

