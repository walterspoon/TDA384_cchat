-module(server).
-export([start/1,stop/1]).

% Start a new server process with the given name
% Do not change the signature of this function.
start(ServerAtom) ->
    % TODO Implement function
    % - Spawn a new process which waits for a message, handles it, then loops infinitely
    Pid = spawn(fun loop/0),
    register(ServerAtom, Pid),
    Pid.

% Continous loop listening for messages
loop() ->
    receive
        {From, Message} ->
            %% Handle the message here
            io:format("Received message: ~p~n", [Message]),
            %% Reply to the client
            From ! {self(), "Default message"},
            loop()
    end.
    % - Register this process to ServerAtom
    % - Return the process ID

% Stop the server process registered to the given name,
% together with any other associated processes
stop(ServerAtom) ->
    % TODO Implement function
    % Return ok
    ServerAtom ! stop,
    ok.
    
