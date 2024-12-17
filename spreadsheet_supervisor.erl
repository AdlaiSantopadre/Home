
-module(spreadsheet_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0, add_spreadsheet/5]).%%,register_owner/2,assign_new_owner/1

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
        {simple_one_for_one, 10, 60},  % simple one-for-one strategy
        [
            {spreadsheet_worker,
             {distributed_spreadsheet, start_link, []},% Il worker sarà avviato dinamicamente
             transient, % Transient: riavvia solo se il worker termina in modo anomalo
             5000,     % Restart timeout
             worker,
             [distributed_spreadsheet]} % Modulo del worker
        ]
    } }.

%% API: avvia un nuovo processo gen_server per gestire uno spreadsheet 
add_spreadsheet(SpreadsheetName, N, M, K, OwnerPid) ->
    Args = [SpreadsheetName, N, M, K, OwnerPid], % Passa tutti i parametri richiesti
    case supervisor:start_child(?MODULE, [Args]) of
        {ok, GenServerPid} ->
            io:format("Spreadsheet ~p created with GenServer PID ~p~n", [SpreadsheetName, GenServerPid]),
            % Save the owner in the state and Mnesia
            case mnesia:transaction(fun() ->
                mnesia:write(#spreadsheet_owners{name = SpreadsheetName, owner = OwnerPid})
            end) of
                {atomic, ok} ->
                    io:format("Owner ~p registered for spreadsheet ~p~n", [OwnerPid, SpreadsheetName]),
                    erlang:monitor(process, OwnerPid), % Monitora direttamente l'OwnerPid
                    {ok, GenServerPid};
                {aborted, Reason} ->
                    io:format("Failed to register owner for spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Failed to create spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
            {error, Reason}
    end.

