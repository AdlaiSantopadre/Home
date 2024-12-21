-module(my_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Avvia Mnesia
    ok = mnesia:start(),
    io:format("Mnesia started~n"),
    %% Avvia il supervisore principale
    app_sup:start_link().

%% Arresta l'applicazione
stop(_State) ->
    %% Termina il supervisore radice
    case whereis(app_sup) of
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

