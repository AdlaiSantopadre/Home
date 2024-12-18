-module(spreadsheet_supervisor).
-behaviour(supervisor).

%% API
-export([start_supervisor/1, terminate_spreadsheet/1, which_children/0]).

%% Supervisor callbacks
-export([init/1]).

%% Avvia il supervisore per uno spreadsheet specifico
start_supervisor([SpreadsheetName, N, M, K, OwnerPid]) ->
    %% Usa il nome dello spreadsheet per creare un supervisore locale
    spreadheet_supervisor:start_link({local, SpreadsheetName}, ?MODULE, {SpreadsheetName, N, M, K, OwnerPid}).

%% Inizializzazione del supervisore
init({SpreadsheetName,N, M, K, OwnerPid}) ->
%% Verifica se il gen_server è già attivo
    case whereis(SpreadsheetName) of
        undefined ->
            
    %% Configura il supervisore con strategia one_for_one
    {ok, {
        {one_for_one, 10, 60},  % Strategia: one_for_one
        [
            {spreadsheet_worker,
             {distributed_spreadsheet, start_link, [[SpreadsheetName, N, M, K, OwnerPid]]},  % Passa il nome come parametro
             transient,
             5000,
             worker,
             [distributed_spreadsheet]}  % Modulo richiesto
        ]
    }};
    _Pid ->
            %% Restituisci errore
            io:format("Spreadsheet ~p already exists.~n", [SpreadsheetName]),
            {stop, already_started}
    end.



%% API: Termina uno spreadsheet
terminate_spreadsheet(SpreadsheetName) ->
    supervisor:terminate_child(SpreadsheetName, SpreadsheetName).

%% API: Lista dei figli del supervisore
which_children() ->
    supervisor:which_children(?MODULE).
