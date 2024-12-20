-module(distributed_spreadsheet).
-behaviour(gen_server).

%% API
-export([start_link/1, new/4]).

%% gen_server callbacks
-export([init/1, terminate/2]).

%% API per creare uno spreadsheet
new(SpreadsheetName, N, M, K) ->
    %% Controlla se il supervisore esiste già
    case whereis(SpreadsheetName) of
        undefined ->
            %% Avvia un nuovo supervisore
             root_supervisor:start_spreadsheet(SpreadsheetName, N, M, K) ;
 %               {ok, _SupervisorPid} ->
  %                  io:format("Spreadsheet ~p and its supervisor started successfully.~n", [SpreadsheetName]),
   %                 {ok, SpreadsheetName};
    %            {error, Reason} ->
     %               io:format("Failed to start supervisor for ~p: ~p~n", [SpreadsheetName, Reason]),
      %              {error, Reason}
       %     end;
        _Pid ->
            %% Se il supervisore esiste già, restituisci errore
            io:format("Spreadsheet ~p already exists.~n", [SpreadsheetName]),
            {error, already_started}
    end.

%% Avvia il gen_server
start_link({SpreadsheetName, N, M, K, OwnerPid}) ->
    gen_server:start_link({global, SpreadsheetName}, ?MODULE, {SpreadsheetName, N, M, K, OwnerPid}, []).

%% gen_server callbacks
%% Inizializzazione dello spreadsheet
init({SpreadsheetName, N, M, K, OwnerPid}) ->
    io:format("Initializing spreadsheet ~p with owner ~p~n", [SpreadsheetName, OwnerPid]),
    {ok, #{name => SpreadsheetName, size => {N, M, K}, owner => OwnerPid}}.

%% Terminazione
terminate(Reason, State) ->
    io:format("Terminating spreadsheet ~p with reason: ~p~n", [maps:get(name, State), Reason]),
    ok.
