-module(distributed_spreadsheet_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, init/1, handle_info/2]).

start_link() ->
    supervisor:start_link({local, distributed_spreadsheet_sup}, ?MODULE, []).

init([]) ->
    %% Start monitoring nodes
    net_kernel:monitor_nodes(true),
    io:format("Monitoring node connections and disconnections~n"),

    %% Define child processes (spreadsheet)
    SpreadsheetSpec = {
        distributed_spreadsheet, %% Child process
        {distributed_spreadsheet, start_link, []}, %% Start function
        permanent, %% Always restart
        5000, %% Restart delay
        worker, %% Process type
        [distributed_spreadsheet] %% Module name
    },
    
    {ok, {{one_for_one, 5, 10}, [SpreadsheetSpec]}}.

%% Handle node up and down messages
handle_info({nodeup, Node}, State) ->
    io:format("Node ~p has joined the cluster~n", [Node]),
    {noreply, State};
    
handle_info({nodedown, Node}, State) ->
    io:format("Node ~p has left the cluster~n", [Node]),
    %% You can trigger recovery logic here
    {noreply, State}.

