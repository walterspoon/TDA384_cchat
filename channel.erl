-module(channel).
-behaviour(gen_server).

-export([start/1, joinChannel/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {clients = []}).

init(_) ->
    {ok, #state{clients = []}}.

start(ChannelName) ->
    gen_server:start_link({local, list_to_atom(ChannelName)}, ?MODULE, {}, []).

joinChannel(Nick, From) ->
    gen_server:cast(self(), {join, Nick, From}).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({join, Nick, From}, State) ->
    NewState = State#state{clients = [{Nick, From} | State#state.clients]},
    {noreply, NewState};
handle_cast({message, Sender, Msg}, State) ->
    BroadcastMsg = {message_receive, Sender, Msg},
    lists:foreach(fun({_, Pid}) -> Pid ! BroadcastMsg end, State#state.clients),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.