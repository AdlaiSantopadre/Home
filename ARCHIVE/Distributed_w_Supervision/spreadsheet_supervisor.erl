-module(spreadsheet_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Avvia il supervisore con parametri
start_link(Args) ->
    case Args of [SpreadsheetName, N, M, K, OwnerPid]->
        io:format("Starting spreadsheet supervisor with params: ~p~n", Args),
        supervisor:start_link({local, supervisor}, ?MODULE, [SpreadsheetName, N, M, K, OwnerPid]);
      _ ->
            {error, invalid_args}
    end.    

%% Inizializzazione del supervisore
init([SpreadsheetName, N, M, K, OwnerPid]) -> Args= [SpreadsheetName, N, M, K, OwnerPid],
    io:format("Initializing supervisor with params: ~p~n", [Args]),
    io:format("Initializing supervisor for spreadsheet ~p~n", [SpreadsheetName]),
    {ok, {
        {one_for_one, 10, 60},
        [
            {spreadsheet_worker,
             {distributed_spreadsheet, start_link, [SpreadsheetName, N, M, K, OwnerPid]},
             transient,
             5000,
             worker,
             [distributed_spreadsheet]}
        ]
    }}.
