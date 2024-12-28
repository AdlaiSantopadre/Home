-module(my_app).
-behaviour(application).
%% Include the record definitions
-include("records.hrl").
-export([start/2, stop/1,restore_spreadsheets/0]).

start(_StartType, _StartArgs) ->
    %% Avvia Mnesia
    application:start(mnesia),
    io:format("Mnesia started~n"),
    %% Avvia il supervisore principale
    app_sup:start_link(),
    restore_spreadsheets().

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
        mnesia:match_object(#spreadsheet_info{name = '_', rows = '_', cols = '_', tabs = '_'})
    end) of
        {atomic, Spreadsheets} ->
            %% Ripristina ogni spreadsheet
            lists:foreach(fun(#spreadsheet_info{name = Name, rows = Rows, cols = Cols, tabs = Tabs}) ->
                case gen_server:start_link({global, Name}, distributed_spreadsheet, {Name, Rows, Cols, Tabs}, []) of
                    {ok, Pid} ->
                        io:format("Restored spreadsheet ~p with PID ~p~n", [Name, Pid]);
                    {error, Reason} ->
                        io:format("Failed to restore spreadsheet ~p: ~p~n", [Name, Reason])
                end
            end, Spreadsheets),
            ok;
        {aborted, Reason} ->
            io:format("Failed to restore spreadsheets: ~p~n", [Reason]),
            {error, Reason}
    end.



