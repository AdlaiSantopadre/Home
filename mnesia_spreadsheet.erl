-module(mnesia_spreadsheet).

-export([new/4, new/1, get/5, set/6, share/2,init/4]).
-include("spreadsheet_data.hrl").

%% Valori di default per righe, colonne e schede introdotti mediante definizione di MACRO
% -define(MACRO_NAME, ReplacementValue).
-define(DEFAULT_ROWS, 5).
-define(DEFAULT_COLS, 10).
-define(DEFAULT_TABS, 3).

%% Funzione per creare un nuovo spreadsheet con valori di default
new(SpreadsheetName) ->
    new(SpreadsheetName, ?DEFAULT_ROWS, ?DEFAULT_COLS, ?DEFAULT_TABS).

%% Funzione per creare un nuovo spreadsheet con N righe, M colonne e K schede


new(SpreadsheetName, N, M, K) ->
    
    mnesia:transaction(fun() ->
        case mnesia:read({spreadsheet_owners, SpreadsheetName}) of
            [] -> 
                init(SpreadsheetName, N, M, K),
                % Registra il nome globale per il processo creatore
                global:register_name({spreadsheet_owner, SpreadsheetName}, self()),
                
                % Salva il nome globale nella tabella spreadsheet_owners
                mnesia:write({spreadsheet_owners, SpreadsheetName, self()}),
                {ok, SpreadsheetName};
            _ -> {error, already_exists}
        end
    end).
init(Name, N, M, K) ->
    % Genera tutti i record per lo spreadsheet
    Records = generate_records(Name, N, M, K),
    io:format("Generati ~p record per lo spreadsheet ~p~n", [length(Records), Name]),
    % Inserisce tutti i record con una transazione Mnesia
    mnesia:transaction(fun() ->
        
        lists:foreach(fun(Record) ->io:format("Inserisco record: ~p~n", [Record]), 
        mnesia:write(Record) end, Records)
    end),
    {ok, Name}.    
generate_records(Name, N, M, K) ->
    % Genera una lista di tutti i record #spreadsheet_data
    [#spreadsheet_data{name = Name, tab = Tab, row = Row, col = Col, value = undef}
     || Tab <- lists:seq(1, K),
        Row <- lists:seq(1, N),
        Col <- lists:seq(1, M)].    

%% Funzione get/5 per leggere una cella
get(Spreadsheet, Tab, I, J, Timeout) ->
    Fun = fun() ->
        case mnesia:read({spreadsheet_data, {Spreadsheet, Tab, I, J}}) of
            [] -> undef;
            [#spreadsheet_data{value = Val}] -> Val
        end
    end,
    case mnesia:transaction(Fun, Timeout) of
        {atomic, Value} -> Value;
        {aborted, _Reason} -> timeout
    end.

%% Funzione set/6 per scrivere una cella passando attraverso il gateway
set(Spreadsheet, Tab, I, J, Value, Timeout) ->
    case spreadsheet_gateway:validate_access(Spreadsheet, self(), write) of
        ok ->
            Fun = fun() ->
                mnesia:write(#spreadsheet_data{name = Spreadsheet, tab = Tab, row = I, col = J, value = Value})
            end,
            case mnesia:transaction(Fun, Timeout) of
                {atomic, ok} -> true;
                {aborted, _Reason} -> timeout
            end;
        Error -> Error
    end.


%% Funzione share/2 per gestire le politiche di accesso
share(Spreadsheet, AccessPolicies) ->
    spreadsheet_gateway:modify_access(Spreadsheet, AccessPolicies).
