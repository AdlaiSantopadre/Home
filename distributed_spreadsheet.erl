-module(distributed_spreadsheet).
-behaviour(gen_server).

%% API
-export([new/4,start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, terminate/2]).

%% Avvia il gen_server
start_link([SpreadsheetName, N, M, K, OwnerPid]) ->
    gen_server:start_link({local, SpreadsheetName}, ?MODULE, {SpreadsheetName, N, M, K, OwnerPid}, []).

%% API per creare uno spreadsheet
new(SpreadsheetName, N, M, K) ->
    case whereis(SpreadsheetName) of
        undefined ->
        case spreadsheet_supervisor:start_supervisor([SpreadsheetName, N, M, K, self()]) of
            {ok, _SupervisorPid} ->
                io:format("Spreadsheet ~p and its supervisor started successfully.~n", [SpreadsheetName]),
                {ok, SpreadsheetName};
            {error, Reason} ->
                io:format("Failed to start supervisor for ~p: ~p~n", [SpreadsheetName, Reason]),
                {error, Reason}
            end;
        Pid ->
            io:format("Spreadsheet ~p already exists with PID ~p.~n", [SpreadsheetName, Pid]),
            {error, already_started}
    end.
%% gen_server callbacks
%% Inizializzazione dello spreadsheet
init({SpreadsheetName, N, M, K, OwnerPid}) ->
    io:format("Initializing spreadsheet ~p with owner ~p~n", [SpreadsheetName, OwnerPid]),
    {ok, #{name => SpreadsheetName, size => {N, M, K}, owner => OwnerPid}}.

%% Risponde a richieste di test
handle_call({test_request, Data}, _From, State) ->
    io:format("Received request: ~p~n", [Data]),
    {reply, ok, State}.

%% Terminazione
terminate(Reason, State) ->
    io:format("Terminating spreadsheet ~p with reason: ~p~n", [maps:get(name, State), Reason]),
    ok.
