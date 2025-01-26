-module(spreadsheet_supervisor).
-behaviour(supervisor).
-include("records.hrl").

%% API & Callbacks
-export([init/1,start_link/0, start_spreadsheet/1, terminate_spreadsheet/1, which_children/0,is_spreadsheet/1,recover_existing_spreadsheets/0]).

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


recover_existing_spreadsheets() ->
    %% Recupera tutti i nomi registrati globalmente
    RegisteredNames = global:registered_names(),
    %% Filtra solo i processi dello spreadsheet
    SpreadsheetNames = [Name || Name <- RegisteredNames, is_spreadsheet(Name)],
    %% Riprende la supervisione per ogni processo
    lists:foreach(fun(Name) ->
        global:unregister_name(Name),
        case mnesia:transaction(fun() ->
            mnesia:match_object(#spreadsheet_info{
                name = Name, rows = '_', cols = '_', tabs = '_', owner = '_'
            })
        end) of
        
     
        {atomic, [{spreadsheet_info,Name,R,C,T,O}]} ->
            %% Avvia un nuovo supervisore
            Args = {Name,R,C,T,O},
            case spreadsheet_supervisor:start_spreadsheet(Args) of
                {ok, Pid} ->
                    io:format("Spreadsheet~p restarted and supervised with PID ~p~n", [Name,Pid]),
                    {ok, Pid};
                {error, Reason} ->
                    io:format("Failed to restart spreadsheet ~p: ~p~n", [Name, Reason]),
                    {error, Reason}
            end;
        {atomic, {error, spreadsheet_not_found}} ->
            io:format("Spreadsheet ~p not found in database~n", [Name]),
            {error, spreadsheet_not_found};
        {aborted, Reason} ->
            io:format("Transaction failed while restarting spreadsheet ~p: ~p~n", [Name, Reason]),
            {error, Reason}
    end
    end,SpreadsheetNames).
is_spreadsheet(Name) ->
    %% Esegui la query all'interno di una transazione
    case
        mnesia:transaction(fun() ->
            %% Legge il record corrispondente
            case mnesia:read(spreadsheet_info, Name) of
                [#spreadsheet_info{name = Name}] -> true;
                [] -> false %% Nessun record trovato             
               
            end
        end)
    of
        {atomic, true} -> true;
        {atomic, false} -> false;
            
        {aborted, Reason} ->
            io:format("Transaction aborted : ~p~n", [Reason]),
            {error, transaction_aborted}
    end.

