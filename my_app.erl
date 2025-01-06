-module(my_app).
-behaviour(application).
%% Include the record definitions
-include("records.hrl").
-export([start/2, stop/1, restore_spreadsheets/0]).

%%CALLBACKS%% VEDI DOC
start(_StartType, _StartArgs) ->
    %% Avvia Mnesia
    ok = mnesia:start(),
    io:format("Mnesia started~n"),
     %ripristina tutti quelli esistenti (in base ai dati di Mnesia)
    case restore_spreadsheets() of
                ok ->
                    io:format("If needed,Spreadsheets restored successfully~n");
                _ ->
                    io:format("Unexpected result from restore_spreadsheets~n")
                    
            end,

    %% Avvia il supervisore principale
    app_sup:start_link().
  
%% Arresta l'applicazione
stop(_State) ->

    io:format("Application my_app stopped successfully.~n"),
    ok.


restore_spreadsheets() ->
    case
        mnesia:transaction(fun() ->
            %% Recupera tutti i record nella tabella spreadsheet_info
            mnesia:match_object(#spreadsheet_info{
                name = '_', rows = '_', cols = '_', tabs = '_', owner = '_'
            })
        end)
    of
        {atomic, []} ->
            io:format("No spreadsheet yet ~n"),
            ok;
        {atomic, Spreadsheets} ->
            lists:foreach(
                fun(
                    #spreadsheet_info{name = Name, rows = Rows, cols = Cols, tabs = Tabs, owner = OwnerPid}
                    ) ->                    
                    io:format("Restoring spreadsheet ~p owned by PID ~p~n", [Name, OwnerPid]),
                    case
                        gen_server:start_link(
                            {global, Name},
                            distributed_spreadsheet,
                            {Name, Rows, Cols, Tabs, OwnerPid},
                            []
                        )
                    of
                        {ok, Pid} ->
                            io:format("Restored spreadsheet ~p with PID ~p~n", [Name, Pid]);
                        {error, Reason} ->
                            io:format("Failed to restore spreadsheet ~p: ~p~n", [Name, Reason])
                    end
                end,
                Spreadsheets
            ),
            ok;
        {aborted, Reason} ->
            io:format("Failed to restore spreadsheets: ~p~n", [Reason]),
            %{error, Reason}Anche in caso di errore, restituisci ok per continuare
            ok
    end.


