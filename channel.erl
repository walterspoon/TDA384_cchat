-module(channel).

-export([start/1, handle_call/2]).

-record(state, {
    clients = [] % list of all channels created so far
}).

initial_state() ->
    #state{
        clients = []
    }.

start(ChannelName) ->
    genserver:start(list_to_atom(ChannelName), initial_state(), fun handle_call/2).

handle_call(State, {join, Client}) ->
    case lists:member(Client, State#state.clients) of
        true ->
            % Client is alredy in channel
            {reply, {error, client_alredy_in_channel}, State};
        false ->
            NewState = State#state{clients = [Client|State#state.clients]},
            {reply, ok, NewState}
    end;

handle_call(State, _) ->
    {reply, ok, State}.