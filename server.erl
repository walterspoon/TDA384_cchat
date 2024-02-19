-module(server).
-export([start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    nicks,   % list of all nicks registered so far
    channels % list of all channels created so far
}).

init(_) ->
    {ok, #state{nicks = [], channels = []}}.

start(ServerAtom) ->
    gen_server:start_link({local, ServerAtom}, ?MODULE, [], []).

handle_call({join, From, Nick, Channel}, _From, State) ->
    Channels = State#state.channels,
    case dict:find(Channel, Channels) of
        {ok, ChanPid} ->
            % The channel exists
            % Let the client join the channel asynchronously
            gen_server:cast(ChanPid, {join, Nick, From}),
            {reply, ok, State};
        error ->
            % The channel does not exist
            % Create a new channel and let the client join the channel asynchronously
            ChanPid = spawn(channel, start, [Channel]),
            NewChannels = dict:store(Channel, ChanPid, Channels),
            NewState = State#state{channels = NewChannels},
            gen_server:cast(ChanPid, {join, Nick, From}),
            {reply, ok, NewState}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

stop(ServerAtom) ->
    gen_server:stop(ServerAtom).