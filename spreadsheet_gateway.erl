-module(spreadsheet_gateway).
-behaviour(gen_server).

-export([start_link/0, validate_access/3, modify_access/2,stop/1,init/1]).
%% gen_server callbacks
-export([handle_call/3, handle_cast/2,terminate/2]).

% Avvio del gen_server
start_link() ->
    gen_server:start_link({global, spreadsheet_gateway}, ?MODULE, [], []).

% Inizializzazione
init([]) ->
    io:format("Inizializzazione del gen_server spreadsheet_gateway~n"),
    {ok, #{}}.
stop(SpreadsheetName) ->
    gen_server:cast({global, SpreadsheetName}, stop).

% Validazione dell'accesso
validate_access(SpreadsheetName, Proc, Action) ->
    gen_server:call({global, spreadsheet_gateway}, {validate_access, SpreadsheetName, Proc, Action}).

% Modifica delle politiche
modify_access(SpreadsheetName, AccessPolicies) ->
    gen_server:call({global, spreadsheet_gateway}, {modify_access, SpreadsheetName, AccessPolicies}).

% Gestione delle richieste

%validazione accesso in scrittura allo spreadsheet
handle_call({validate_access, SpreadsheetName, Proc, Action}, _From, State) ->
    case check_access(SpreadsheetName, Proc, Action) of
        ok -> {reply, ok, State};
        Error -> {reply, Error, State}
    end;
%gestione aggiornamento Access Policies
handle_call({modify_access, SpreadsheetName, AccessPolicies}, _From, State) ->
    % Verifica se il processo chiamante è il proprietario
    case validate_or_reassign_owner(SpreadsheetName, self()) of
        ok ->
            % Proprietario verificato, aggiorna le politiche
            Result = update_access_policies(SpreadsheetName, AccessPolicies),
            {reply, Result, State};
            
        {error, Reason} ->
                    io:format("Errore durante modify_access: ~p~n", [Reason]),
                    {reply, {error, Reason}, State}
    end.

handle_cast(stop, State) ->
    io:format("Stopping the gen_server process~n"),
    {stop, normal, State}.
%% Clean up when the process stops
terminate(Reason, _State) ->
    io:format("Termine del gen_server spreadsheet_gateway con motivo: ~p~n", [Reason]),
    ok.
validate_or_reassign_owner(SpreadsheetName, Proc) ->
    case check_owner(SpreadsheetName, Proc) of
        ok -> 
            ok; % Il processo è già il proprietario
        {error, no_owner} -> 
            % Tenta di riassegnare il proprietario e verifica di nuovo
            reassign_owner(SpreadsheetName),
            check_owner(SpreadsheetName, Proc);
        Error -> 
            Error % Restituisce altri errori (es. not_owner)
    end.
reassign_owner(SpreadsheetName) ->
    case mnesia:transaction(fun() ->
        mnesia:read(spreadsheet_owners, SpreadsheetName)
    end) of
        {atomic, [{_, {spreadsheet_owner, SpreadsheetName}}]} ->
            global:register_name({spreadsheet_owner, SpreadsheetName}, self()),
            io:format("Proprietario dello spreadsheet ~p riassegnato al processo ~p~n", [SpreadsheetName, self()]);
        _ ->
            io:format("Impossibile riassegnare il proprietario: spreadsheet ~p non trovato~n", [SpreadsheetName])
    end.

% Controlla se il processo è il proprietario dello spreadsheet usando il nome globale registrato
check_owner(SpreadsheetName, Proc) ->
    case mnesia:transaction(fun() ->
        case mnesia:read(spreadsheet_owners, SpreadsheetName) of
            [{_, {spreadsheet_owner, SpreadsheetName}}] ->
                Owner = global:whereis_name({spreadsheet_owner, SpreadsheetName}),
                case Owner of
                    Proc -> ok;
                    undefined -> {error, no_owner};
                    _ -> {error, not_owner}
                end;
            _ -> {error, no_owner}
        end
    end) of
        {atomic, ok} -> ok;
        {atomic, {error, Reason}} -> {error, Reason};
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

