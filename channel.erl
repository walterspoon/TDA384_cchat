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
    % Case to see if client is already in channel
    case lists:member(Client, State#state.clients) of
        true ->
            % Client is alredy in channel
            io:fwrite(" Already in channel! "),
            {reply, {error, user_already_joined, "You are already in channel!"}, State};
        false ->
            NewState = State#state{clients = [Client|State#state.clients]},
            io:fwrite(" Creating the channel! "),
            {reply, ok, NewState}
    end;

handle_call(State, {leave, Client}) -> 
    % Case to see if client is in channel
    case lists:member(Client, State#state.clients) of
        true ->
            % Client is in channel
            NewState = State#state{clients = lists:delete(Client, State#state.clients)},
            io:fwrite(" Leaving the channel! "),
            {reply, ok, NewState};
        false ->
            % Client is not in channel
            io:fwrite(" Not in channel! "),
            {reply, {error, user_not_joined, "You are not in channel!"}, State}
    end;

handle_call(State, _) ->
    {reply, ok, State}.