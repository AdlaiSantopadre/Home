-module(spreadsheet_gateway).
-behaviour(gen_server).

-export([start_link/0, validate_access/3, modify_access/2,stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3,handle_cast/2,terminate/2]).

% Avvio del gen_server
start_link() ->
    gen_server:start_link({local, spreadsheet_gateway}, ?MODULE, [], []).

% Inizializzazione
init([]) ->
    {ok, #{}}.
stop(SpreadsheetName) ->
    gen_server:cast({global, SpreadsheetName}, stop).
% Validazione dell'accesso
validate_access(SpreadsheetName, Proc, Action) ->
    gen_server:call(?MODULE, {validate_access, SpreadsheetName, Proc, Action}).

% Modifica delle politiche
modify_access(SpreadsheetName, AccessPolicies) ->
    gen_server:call(?MODULE, {modify_access, SpreadsheetName, AccessPolicies}).

% Gestione delle richieste

%validazione accesso in scrittura allo spreadsheet
handle_call({validate_access, SpreadsheetName, Proc, Action}, _From, State) ->
    case check_access(SpreadsheetName, Proc, Action) of
        ok -> {reply, ok, State};
        Error -> {reply, Error, State}
    end;
%gestione aggiornamento Access Policies
handle_call({modify_access, SpreadsheetName, AccessPolicies}, _From, State) ->
    case check_owner(SpreadsheetName, self()) of
        ok ->
            Result = update_access_policies(SpreadsheetName, AccessPolicies),
            {reply, Result, State};
        {error, Reason} -> {reply, {error, Reason}, State}
    end.

handle_cast(stop, State) ->
    io:format("Stopping the gen_server process~n"),
    {stop, normal, State}.
%% Clean up when the process stops
terminate(_Reason, _State) ->
    ok.

% Controlla se il processo è il proprietario dello spreadsheet
check_owner(SpreadsheetName, Proc) ->
    case mnesia:transaction(fun() ->
        case mnesia:read({spreadsheet_owners, SpreadsheetName}) of
            [{_, Owner}] when Owner == Proc -> ok;
            _ -> {error, not_owner}
        end
    end) of
        {atomic, ok} -> ok;
        _ -> {error, not_owner}
    end.

% Controlla se il processo ha i permessi richiesti
check_access(SpreadsheetName, Proc, Action) ->
    case mnesia:transaction(fun() ->
        case mnesia:read({access_policies, {SpreadsheetName, Proc}}) of
            [{_, _, Access}] when Access == Action; Access == write -> ok;
            _ -> {error, access_denied}
        end
    end) of
        {atomic, ok} -> ok;
        _ -> {error, access_denied}
    end.

% Aggiorna le politiche di accesso
update_access_policies(SpreadsheetName, AccessPolicies) ->
    mnesia:transaction(fun() ->
        % Rimuovi le vecchie politiche
        mnesia:delete({access_policies, SpreadsheetName}),
        % Inserisci le nuove politiche
        lists:foreach(fun({Proc, Access}) ->
            mnesia:write({access_policies, {SpreadsheetName, Proc, Access}})
        end, AccessPolicies),
        ok
    end).

