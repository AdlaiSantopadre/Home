-module(spreadsheet_supervisor).
-behaviour(supervisor).

%% API & Callbacks
-export([start_link/0, start_link/1, init/1]).

%% Start without parameters
start_link() ->
    %% Use a default name for general supervision
    supervisor:start_link({local, spreadsheet_sup}, ?MODULE, []).


start_link({SpreadsheetName, N, M, K, OwnerPid}) ->
    %% Nome del supervisore derivato dallo SpreadsheetName
    SupervisorName = list_to_atom("supervisore_" ++ atom_to_list(SpreadsheetName)),
    io:format("Supervisor name is ~p~n",[SupervisorName]),
    supervisor:start_link({local, SupervisorName}, ?MODULE, {SpreadsheetName, N, M, K, OwnerPid}).

%% Supervisor initialization
init([]) ->
    %% Default setup for the app_sup context
    {ok, {
        {simple_one_for_one, 10, 60},
        [
            {spreadsheet_worker,
             {distributed_spreadsheet, start_link, []},
             transient,
             5000,
             worker,
             [distributed_spreadsheet]}
        ]
    }};
init({SpreadsheetName, N, M, K, OwnerPid}) ->
    %% Specialized setup for SpreadsheetName
    io:format("Initializing supervisor for spreadsheet ~p~n", [SpreadsheetName]),
    {ok, {
        {simple_one_for_one, 10, 60},
        [
            {spreadsheet_worker,
             {distributed_spreadsheet, start_link, [{SpreadsheetName, N, M, K, OwnerPid}]},
             transient,
             5000,
             worker,
             [distributed_spreadsheet]}
        ]
    }}.