-module(root_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0, start_spreadsheet/4]).

%% Supervisor callbacks
-export([init/1]).

%% Avvia il supervisore root
start_link() ->
    supervisor:start_link({local, root_supervisor}, ?MODULE, []).

%% Inizializza il supervisore root
init([]) ->
    {ok, {
        {simple_one_for_one, 10, 60},
        [
            {spreadsheet_supervisor,
             {spreadsheet_supervisor, start_link, []},
             transient,
             5000,
             supervisor,
             [spreadsheet_supervisor]}
        ]
    }}.

%% API: Avvia uno spreadsheet
start_spreadsheet(SpreadsheetName, N, M, K) -> 
    Args = [SpreadsheetName, N, M, K, self()],  % Prepara gli argomenti come lista
    io:format("Starting child with params: ~p~n", [Args]),
    case supervisor:start_child(root_supervisor, {SpreadsheetName, N, M, K, self()}) of
        {ok, Pid} ->
            io:format("Started spreadsheet supervisor for ~p with PID ~p~n", [SpreadsheetName, Pid]),
            {ok, Pid};
        {error, Reason} ->
            io:format("Failed to start spreadsheet supervisor for ~p: ~p~n", [SpreadsheetName, Reason]),
            {error, Reason}
    end.
