-module(server).
-export([start/1,stop/1,handle/2]).

% Start a new server process with the given name
% Do not change the signature of this function.

-record(state, {
    nicks,   % list of all nicks registered so far
    channels % list of all channels created so far
}).

initial_state() ->
    #state{
        nicks = [],
        channels = []
    }.

start(ServerAtom) ->
    genserver:start(ServerAtom, initial_state(), fun handle/2).

handle(State, {join, Channel, Nick}) ->
    %% Handle the join command here.
    %% Add the channel to the list of channels in the state.
    % Server ska bara hålla koll på vilka kanaler som finns.
    % Finns kanalen som en klient vill joina så låter servern klienten joina kanalen.
    % Finns INTE kanalen så skapar servern en ny kanal (process) och låter klienten joina kanalen.

    case lists:member(Channel,State#state.channels) of
        true ->
            % Kanalen existerar
            % Låt klienten joina kanalen
            channel:joinChannel(Nick, State),
            {reply, ok, State};
        false ->
            % Kanalen finns INTE
            % Skapa en ny kanal och låt klienten joina kanalen
            channel:start(Channel),
            channel:joinChannel(Nick, State),
            NewState = State#state{channels = [Channel | State#state.channels]},
            {reply, ok, NewState}
    end.

%handle(State, Message) ->
%    %% Handle other messages here.
%    %% Log the message and leave the state unchanged.
%    io:format("Received message: ~p~n", [Message]),
%    {reply, ok, State}.


% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    genserver:stop(ServerAtom).
    
