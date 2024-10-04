-module(distributed_processes_utility).
-export([spawn_and_register_processes/1, spawn_process_on_node/2]).

%% Function to spawn and register processes on different nodes
%% Takes a list of {Name, Node} tuples.
spawn_and_register_processes(NamesAndNodes) ->
    lists:foreach(fun({Name, Node}) -> spawn_process_on_node(Name, Node) end, NamesAndNodes).

%% Helper function to spawn a process on a given node and register it globally
spawn_process_on_node(Name, Node) when is_atom(Name), is_atom(Node)->
    %% Spawn the process on the specified Node
    io:format("spawn Name ~p on node ~p~n", [Name, Node]),
    Pid = spawn(Node, fun() -> process_loop() end),
    io:format("Attempting to register Name: ~p with Pid: ~p on Node: ~p~n", [Name, Pid, Node]),
    %% Register the process globally
    global:register_name(Name, Pid),
    
    io:format("Spawned and registered process ~p on node ~p with PID ~p~n", [Name, Node, Pid]).

%% Simple process loop function that logs received messages
process_loop() ->
    receive
        Message -> 
            io:format("Received message: ~p~n", [Message]),
            process_loop()
    end.
