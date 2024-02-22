-module(channel).

-export([start/1, handle_call/2, stop/1]).

-record(state, {
    clients,        % list of all channels created so far
    channelName     % Name of the channel
}).

initial_state(ChannelName) ->  % Initial state
    #state{
        clients = [],
        channelName = ChannelName
    }.

% Send request via genserver
sendRequest(SendToPid, Request) ->
    try genserver:request(SendToPid, Request) of
        Response -> Response    % Return the response from the genserver
    catch 
        timeout_error -> {error, server_not_reached, "Server not reached!"}
    end.

% Start channel process via genserver
start(ChannelName) ->
    genserver:start(list_to_atom(ChannelName), initial_state(ChannelName), fun handle_call/2).

handle_call(State, {join, Client}) ->
    % Case to see if client is already in channel
    case lists:member(Client, State#state.clients) of
        true ->
            % Client is alredy in channel
            {reply, {error, user_already_joined, "You are already in channel!"}, State};
        false ->
            % Add client to clients
            NewState = State#state{clients = [Client|State#state.clients]},
            {reply, ok, NewState}
    end;

handle_call(State, {leave, Client}) -> 
    % Case to see if client is in channel
    case lists:member(Client, State#state.clients) of
        true ->
            % Client is in channel, remove client from clients
            NewState = State#state{clients = lists:delete(Client, State#state.clients)},
            {reply, ok, NewState};
        false ->
            % Client is not in channel
            {reply, {error, user_not_joined, "You are not in channel!"}, State}
    end;

handle_call(State, {message, From, Nick, Msg}) ->
    % Case to see if client is in channel
    case lists:member(From, State#state.clients) of
        true ->
            % Client is in channel
            ClientsWithoutFrom = lists:delete(From, State#state.clients),
            % Get the channel name from record
            ChannelName = State#state.channelName,
            % For each client in channel exept for the message sender,
            % spawn a process sending the message to a client
            lists:foreach(fun(Client) -> 
                spawn(fun() ->
                    sendRequest(Client, {message_receive, ChannelName, Nick, Msg})
                end) end, ClientsWithoutFrom),
            {reply, ok, State};
            
        false ->
            % Client is not in channel
            {reply, {error, user_not_joined, "You are not in channel!"}, State}
    end;

% Catch case when nothing pattern matches
handle_call(State, _) ->
    {reply, ok, State}.

% Stop channel process via genserver
stop(Channel) ->
    genserver:stop(Channel),
    ok.