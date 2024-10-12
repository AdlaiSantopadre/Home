-module(distributed_spreadsheet_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

%% Start the supervisor
start_link() ->
    supervisor:start_link({global, distributed_spreadsheet_sup}, ?MODULE, []).

%% Initialize the supervisor
init([]) ->
    %% Define child processes to be supervised
    SpreadsheetSpec = {
        distributed_spreadsheet, %% The child process (our gen_server)
        {distributed_spreadsheet, start_link, []}, %% Start function
        permanent, %% If the process dies, always restart it
        5000, %% Time to wait before restarting
        worker, %% The type of process (worker, not supervisor)
        [distributed_spreadsheet] %% The module
    },

    %% Strategy: one-for-one, if one child dies, restart only that child
    {ok, {{one_for_one, 10, 10}, [SpreadsheetSpec]}}.
