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
%%%%%Aggiunta
    case application_controller:get_master(my_app) of
    MasterPid when is_pid(MasterPid) ->
                
                %% Crea un atomo unico per il nodo
                GlobalName = list_to_atom("nodo" ++ atom_to_list(node())),
                %% Registra globalmente il MasterPid
                case global:register_name(GlobalName, MasterPid) of
                    yes ->
                        io:format("MasterPid ~p registered globally as ~p~n", [MasterPid, GlobalName]);
                        %{GlobalName, read}; %tutti i nodo del cluster vengono inseriti con policy read
                    no ->
                        io:format("Failed to register MasterPid ~p: ~n", [MasterPid])
                        %{GlobalName,error}
                end;
            undefined ->
                io:format("Failed to retrieve MasterPid ~n", [])
                %{error, undefined}
        end,
%%% Fine aggiunta
    %% Avvia il supervisore principale
    app_sup:start_link().
  
%% Arresta l'applicazione
stop(_State) ->

    % %% Termina il supervisore radice
    % case global:whereis_name(app_sup) of
    %     undefined ->
    %         io:format("Supervisor app_sup not running.~n"),
    %         ok;
    %     Pid ->
    %         io:format("Stopping supervisor app_sup with PID ~p~n", [Pid]),
    %         exit(Pid, kill)
    % end,
    % %% Ferma Mnesia
    % case mnesia:stop() of
    %     ok ->
    %         io:format("Mnesia stopped successfully.~n"),
    %         ok;
    %     {error, not_started} ->
    %         io:format("Mnesia was not running.~n"),
    %         ok
    % end,
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


