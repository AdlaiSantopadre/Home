-module(my_app).
-behaviour(application).
%% Include the record definitions
-include("records.hrl").
-export([start/2, stop/1,restore_spreadsheets/0,get_owner_pid/1]).

start(_StartType, _StartArgs) ->
    %% Avvia Mnesia
    application:start(mnesia),
    io:format("Mnesia started~n"),
    %% Avvia il supervisore principale
    case app_sup:start_link() of
        {ok, _Pid} ->
            io:format("Supervisor started~n"),
            case restore_spreadsheets() of%ripristina tutti quelli esistenti (in base ai dati di Mnesia)

                ok ->
                    io:format("Spreadsheets restored successfully~n"),
                    ok;
                _ ->
                    io:format("Unexpected result from restore_spreadsheets~n"),
                    ok  %% Continua comunque
            end;
        {error, Reason} ->
            io:format("Failed to start supervisor: ~p~n", [Reason]),
            {error, Reason}
    end. 
%% Arresta l'applicazione
stop(_State) ->
    %% Termina il supervisore radice
    case global:whereis_name(app_sup) of
        undefined ->
            io:format("Supervisor app_sup not running.~n"),
            ok;
        Pid ->
            io:format("Stopping supervisor app_sup with PID ~p~n", [Pid]),
            exit(Pid, kill)
    end,
    %% Ferma Mnesia
    case mnesia:stop() of
        ok ->
            io:format("Mnesia stopped successfully.~n"),
            ok;
        {error, not_started} ->
            io:format("Mnesia was not running.~n"),
            ok
    end,
    io:format("Application my_app stopped successfully.~n"),
    ok.
restore_spreadsheets() ->
    case mnesia:transaction(fun() ->
        %% Recupera tutti i record nella tabella spreadsheet_info
        mnesia:match_object(#spreadsheet_info{name = '_', rows = '_', cols = '_', tabs = '_'})
    end) of
        {atomic, Spreadsheets} ->
            lists:foreach(fun(#spreadsheet_info{name = Name, rows = Rows, cols = Cols, tabs = Tabs}) ->
                case get_owner_pid(Name) of
                    {ok, OwnerPid} ->
                        io:format("Restoring spreadsheet ~p owned by PID ~p~n", [Name, OwnerPid]),
                        case gen_server:start_link({global, Name}, distributed_spreadsheet, {Name, Rows, Cols, Tabs, OwnerPid}, []) of
                            {ok, Pid} ->
                                io:format("Restored spreadsheet ~p with PID ~p~n", [Name, Pid]);
                            {error, Reason} ->
                                io:format("Failed to restore spreadsheet ~p: ~p~n", [Name, Reason])
                        end;
                    {error, not_found} ->
                        io:format("No owner found for spreadsheet ~p~n", [Name]);
                    {error, Reason} ->
                        io:format("Error fetching owner for spreadsheet ~p: ~p~n", [Name, Reason])
                end
            end, Spreadsheets),
            ok;
        {aborted, Reason} ->
            io:format("Failed to restore spreadsheets: ~p~n", [Reason]),
            ok %{error, Reason}Anche in caso di errore, restituisci ok per continuare
    end.

get_owner_pid(Name) ->
    %% Esegui la query all'interno di una transazione
    case mnesia:transaction(fun() ->
        %% Legge il record corrispondente
        case mnesia:read(spreadsheet_owners, Name) of
            [#spreadsheet_owners{owner = OwnerPid}] ->
                {ok, OwnerPid};  %% Record trovato
            [] ->
                {error, not_found}  %% Nessun record trovato
        end
    end) of
        {atomic, Result} ->
            Result;
        {aborted, Reason} ->
            io:format("Transaction aborted while fetching owner PID: ~p~n", [Reason]),
            {error, transaction_aborted}
    end.



