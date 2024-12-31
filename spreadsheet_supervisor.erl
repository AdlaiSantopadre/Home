-module(spreadsheet_supervisor).
-behaviour(supervisor).

%% API & Callbacks
-export([init/1,start_link/0, start_spreadsheet/1, terminate_spreadsheet/1, which_children/0]).

%% Avvia il supervisore per i  gen_server

start_link() ->    
    supervisor:start_link({local, spreadsheet_sup}, ?MODULE, []).

%% API per avviare dinamicamente un figlio
start_spreadsheet(Args) ->
    %% Avvia un nuovo figlio per lo spreadsheet
    io:format("Passing params to start_child: ~p~n", [Args]),
    supervisor:start_child(spreadsheet_sup, [Args]).

%% API per terminare un figlio
terminate_spreadsheet(SpreadsheetName) ->
    case whereis(SpreadsheetName) of
        undefined ->
            io:format("Spreadsheet ~p not found~n", [SpreadsheetName]),
            {error, not_found};
        Pid ->
            io:format("Terminating spreadsheet ~p with PID ~p~n", [SpreadsheetName, Pid]),
            exit(Pid, kill),
            ok
    end.

%% API per elencare i figli
which_children() ->
    supervisor:which_children(spreadsheet_sup).




%% Inizializzazione del supervisore
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
    }}.
%init({SpreadsheetName, N, M, K, OwnerPid}) ->
%    %% Specialized setup for SpreadsheetName
%    io:format("Initializing supervisor for spreadsheet ~p~n", [SpreadsheetName]),
%    {ok, {
%        {simple_one_for_one, 10, 60},
%        [
%            {spreadsheet_worker,
%             {distributed_spreadsheet, start_link, [{SpreadsheetName, N, M, K, OwnerPid}]},
%             transient,
%             5000,
%             worker,
%             [distributed_spreadsheet]}
%        ]
%    }}.