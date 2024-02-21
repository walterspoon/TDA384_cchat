-module(channel).

-export([start/1, handle_call/2]).

-record(state, {
    clients = [], % list of all channels created so far
    channelName
}).

initial_state(ChannelName) ->  % ChannelName is now a parameter of the function
    #state{
        clients = [],
        channelName = ChannelName
    }.

% Send request via genserver
sendRequest(RecieverPid, Request) ->
    try genserver:request(RecieverPid, Request) of
        Response -> Response
    catch
        timeout_error -> {error, server_not_reached, "Server not reached!"}
    end.

start(ChannelName) ->
    genserver:start(list_to_atom(ChannelName), initial_state(ChannelName), fun handle_call/2).

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

handle_call(State, {message, From, Nick, Msg}) ->
    % Case to see if client is in channel
    case lists:member(From, State#state.clients) of
        true ->
            % Client is in channel
            ClientsWithoutFrom = lists:delete(From, State#state.clients),
            % Get the channel name from record
            ChannelName = State#state.channelName,
            % For each client in channel exept for the message sender
            lists:foreach(fun(Client) -> 
                sendRequest(Client, {message_receive, ChannelName, Nick, Msg})
            end, ClientsWithoutFrom),
            {reply, ok, State};
        false ->
            % Client is not in channel
            io:fwrite(" Not in channel! "),
            {reply, {error, user_not_joined, "You are not in channel!"}, State}
    end;

handle_call(State, _) ->
    {reply, ok, State}.