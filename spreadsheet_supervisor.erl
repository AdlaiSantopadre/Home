-module(spreadsheet_supervisor).
-behaviour(supervisor).

%% API
-export([start_supervisor/1, terminate_spreadsheet/1, which_children/0]).

%% Supervisor callbacks
-export([init/1]).

%% API: Avvia il supervisore di uno spreadsheet specifico in modo indipendente 
start_supervisor([SpreadsheetName, N, M, K, OwnerPid]) ->
    case whereis(supervisor) of
        undefined ->
            erlang:spawn(fun() -> 
                supervisor:start_link({local, supervisor}, ?MODULE, {SpreadsheetName, N, M, K, OwnerPid}) 
            end),
            io:format("Supervisor for spreadsheet ~p started independently~n", [SpreadsheetName]),
            ok;
        _Pid ->
            io:format("Supervisor for spreadsheet ~p already running~n", [SpreadsheetName]),
            {error, already_started}
    end.

%% Inizializzazione del supervisore
init({SpreadsheetName, N, M, K, OwnerPid}) ->
    io:format("Initializing supervisor for spreadsheet ~p~n", [SpreadsheetName]),
    %% Configura il supervisore con strategia one_for_one
    {ok, {
        {one_for_one, 10, 60},
        [
            {spreadsheet_worker,
             {distributed_spreadsheet, start_link, [{SpreadsheetName, N, M, K, OwnerPid}]},
             transient,
             5000,
             worker,
             [distributed_spreadsheet]}
        ]
    }}.

%% API: Termina uno spreadsheet
terminate_spreadsheet(SpreadsheetName) ->
    case whereis(SpreadsheetName) of
        undefined ->
            io:format("No supervisor found for ~p~n", [SpreadsheetName]),
            {error, not_found};
        Pid ->
            io:format("Terminating supervisor for ~p with PID ~p~n", [SpreadsheetName, Pid]),
            exit(Pid, kill),
            ok
    end.

%% API: Lista dei figli del supervisore
which_children() ->
    supervisor:which_children(?MODULE).
