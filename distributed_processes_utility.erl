-module(distributed_processes_utility).
-export([register_self/1, register_process_on_node/2]).

%% Function to register processes (self()) on different nodes
%% Takes a list of {Name, Node} tuples, but registers self() with the given Name
register_self(NamesAndNodes) ->
    lists:foreach(fun({Name, Node}) -> register_process_on_node(Name, Node) end, NamesAndNodes).

%% Helper function to register the current process (self()) globally on a given node
register_process_on_node(Name, Node) when is_atom(Node) -> % ho tolto la guard su Name
    %% Ensure the process is running on the correct node
    %% Use node() to check if we're on the right node
    case node() of
        Node ->
            %% If on the correct node, register the process globally
            io:format("Registering self() as ~p on node ~p~n", [Name, Node]),
            global:register_name(Name, self()),
            io:format("Successfully registered self() as ~p globally on node ~p~n", [Name, Node]);
        _ ->
            io:format("Not on the specified node ~p, skipping registration for ~p~n", [Node, Name])
    end.

