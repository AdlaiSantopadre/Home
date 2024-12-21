
-module(spreadsheet_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0, add_spreadsheet/4]).%%,register_owner/2,assign_new_owner/1

%% Supervisor callbacks
-export([init/1]).%% handle_info/2,handle_call/3

%% Include the record definition
-include("records.hrl").


%% Start the supervisor
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor initialization
init([]) ->
    {ok, {
        {one_for_one, 10, 60},  % one-for-one strategy
        [
            {spreadsheet_worker,
             {distributed_spreadsheet, start_spreadsheet, []},%  senza parametri iniziali
             permanent, 
             5000,     % Restart timeout
             worker,
             [distributed_spreadsheet]} % Modulo del worker
        ]
    } }.

%% API: avvia un nuovo processo gen_server per gestire uno spreadsheet 
add_spreadsheet(SpreadsheetName, N, M, K) ->
            %% Creiamo un ID unico per il worker
            WorkerId = list_to_atom(SpreadsheetName),
            Args = [SpreadsheetName, N, M, K, self()], % Passa tutti i parametri richiesti
    case supervisor:start_child(?MODULE, {spreadsheet_worker, Args}) of
        {ok, GenServerPid} ->
            io:format("Spreadsheet ~p creating with GenServer PID ~p~n", [SpreadsheetName, GenServerPid]),
            gen_server:call(GenServerPid, {init_spreadsheet, SpreadsheetName, N, M, K,self()});
        {error, Reason} ->
            io:format("Failed to create spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
            {error, Reason}
    end.

