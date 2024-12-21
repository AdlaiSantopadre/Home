-module(distributed_processes_sup_node1).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1, handle_info/2, terminate/2]).

%% Start the supervisor
start_link() ->
    supervisor:start_link({local, distributed_processes_sup_node1}, ?MODULE, []).

%% Initialize the supervisor and start monitoring nodes
init([]) ->
    %% Start monitoring node connections and disconnections
    net_kernel:monitor_nodes(true),
    io:format("Monitoring node connections and disconnections~n"),

    %% Define child processes to be supervised

    %% Process 1 (proc1) running on node1
    Proc1Spec = {
        proc1,
        {distributed_process_utility, start_link, [proc1]},
        permanent,  % Always restart
        5000,  % Restart delay
        worker,  % Process type
        [distributed_process_utility]
    },

    %% distributed_spreadsheet process
    SpreadsheetSpec = {
        distributed_spreadsheet,
        {distributed_spreadsheet, start_link, []},
        permanent,
        5000,
        worker,
        [distributed_spreadsheet]
    },

    %% Strategy: one_for_one means restart the failed process only
    {ok, {{one_for_one, 5, 10}, [Proc1Spec, SpreadsheetSpec]}}.

%% Handle node up and node down messages
handle_info({nodeup, Node}, State) ->
    io:format("Node ~p has joined the cluster~n", [Node]),
    {noreply, State};

handle_info({nodedown, Node}, State) ->
    io:format("Node ~p has left the cluster~n", [Node]),

    %% If node1 goes down, handle failover for distributed_spreadsheet ownership transfer
    case node(Node) of
        'node1@DESKTOPQ2A2FL7' ->
            io:format("Node1 has failed. Transferring ownership...~n"),
            %% Call transfer_ownership to transfer the ownership to another node
            transfer_ownership(distributed_spreadsheet);
        _ ->
            %% No action required for other nodes
            io:format("Another node failed, no critical action required.~n")
    end,
    {noreply, State}.

%% Handle termination and stop node monitoring
terminate(_Reason, _State) ->
    io:format("Stopping node monitoring.~n"),
    net_kernel:monitor_nodes(false),
    ok.



