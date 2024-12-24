-module(distributed_spreadsheet). % gen_server with Mnesia
-behaviour(gen_server).
%% API functions exported from assignement
-export([new/1, new/4, share/2]).%, start_spreadsheet/0
%% API
-export([start_link/1]).
%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Include the record definitions
-include("records.hrl").

%%% API FUNCTIONS %%%

%% Crea uno spreadsheet con valori di default
new(Name) ->
    N = 3,  % Default N (numero di righe)
    M = 4,  % Default M (numero di colonne)
    K = 2,   % Default K (numero di tab)
    new(Name, N, M, K).

%% Crea uno spreadsheet con parametri specifici
new(SpreadsheetName, N, M, K) ->
    %% Invio richiesta ad app_sup per creare il supervisore specifico
    OwnerPid= application_controller:get_master(my_app),
    Args= {SpreadsheetName, N, M, K,OwnerPid}, 
    case spreadsheet_supervisor:start_spreadsheet(Args) of
        {ok, Pid} ->
            io:format("Spreadsheet ~p started successfully with PID ~p~n", [SpreadsheetName, Pid]),
            {ok, Pid};
        {error, Reason} ->
            io:format("Failed to start spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
            {error, Reason}
    end.

%% Avvia il gen_server
start_link(Args) ->
    io:format("Starting distributed_spreadsheet with args: ~p~n", [Args]),
    {SpreadsheetName, _, _, _, _} = Args,
    gen_server:start_link({global, SpreadsheetName}, ?MODULE, Args, []).

%% Inizializza il processo spreadsheet distribuito
init({SpreadsheetName, N, M, K, OwnerPid}) ->
    io:format("Initializing spreadsheet ~p with owner ~p~n", [SpreadsheetName, OwnerPid]),
%%
    case mnesia:transaction(fun() ->
        case mnesia:read({spreadsheet_owners, SpreadsheetName}) of
            [] -> %% New spreadsheet creation
                io:format("No existing data for spreadsheet ~p. Initializing records.~n", [SpreadsheetName]),
                Records = generate_records(SpreadsheetName, N, M, K),
                lists:foreach(fun(Record) -> mnesia:write(Record) end, Records),
                mnesia:write(#spreadsheet_owners{name = SpreadsheetName, owner = OwnerPid}),
                {new, ok};
            [#spreadsheet_owners{owner = OwnerPid}] -> %% Restart case
                io:format("Found existing owner ~p for spreadsheet ~p.~n", [OwnerPid, SpreadsheetName]),
                {existing, OwnerPid}
        end
    end) of
        {atomic, {new, ok}} ->
            {ok, #{name => SpreadsheetName, size => {N, M, K}, owner => OwnerPid}}; %% OK
        {atomic, {existing, ExistingOwner}} ->
            {ok, #{name => SpreadsheetName, size => {N, M, K}, owner => ExistingOwner}};
        {aborted, Reason} ->
            io:format("Failed to initialize spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
            {stop, Reason}
    end.


%%% gen_server CALLBACKS %%%

terminate(Reason, State) ->
    io:format("Terminating spreadsheet ~p with reason: ~p~n", [maps:get(name, State), Reason]),
    ok.
%% Callbacks NON IMPLEMENTATE
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported_operation}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% FUNZIONI AUSILIARIE

%%genera tutti i record necessari per rappresentare lo spreadsheet. Usa list comprehensions per creare i record.
generate_records(Name, N, M, K) ->
    [#spreadsheet_data{name = Name, tab = Tab, row = Row, col = Col, value = undef}
     || Tab <- lists:seq(1, K),
        Row <- lists:seq(1, N),
        Col <- lists:seq(1, M)].

