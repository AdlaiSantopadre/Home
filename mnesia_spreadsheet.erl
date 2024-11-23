-module(mnesia_spreadsheet). 
-export([new/4,new/1, get/5, set/6, share/2]).
%% Valori di default per righe, colonne e schede introdotti mediante definizione di MACRO
% -define(MACRO_NAME, ReplacementValue).
-define(DEFAULT_ROWS, 5).
-define(DEFAULT_COLS, 10).
-define(DEFAULT_TABS, 3).
%% Funzione per creare un nuovo spreadsheet con N righe, M colonne e K schede
%% Funzione per creare un nuovo spreadsheet con valori di default
new(SpreadsheetName) ->
    new(SpreadsheetName, ?DEFAULT_ROWS, ?DEFAULT_COLS, ?DEFAULT_TABS).


new(SpreadsheetName, N, M, K) ->
    mnesia:transaction(fun() ->
        case mnesia:read({spreadsheet_data, SpreadsheetName}) of
            [] -> 
                init_spreadsheet(SpreadsheetName, N, M, K);
            _ -> {error, already_exists}
        end
    end).

init_spreadsheet(Name, N, M, K) ->
    lists:foreach(fun(Tab) ->
        lists:foreach(fun(Row) ->
            lists:foreach(fun(Col) ->
                mnesia:write({spreadsheet_data, Name, Tab, Row, Col, undef})
            end, lists:seq(1, M))
        end, lists:seq(1, N))
    end, lists:seq(1, K)),
    {ok, Name}.

%% Funzione get/6 per leggere una cella
get(Spreadsheet, Tab, I, J, Timeout) ->
    Fun = fun() ->
        case mnesia:read({spreadsheet_data, {Spreadsheet, Tab, I, J}}) of
            [] -> undef;
            [{_, _, _, _, Value}] -> Value
        end
    end,
    case mnesia:transaction(Fun, Timeout) of
        {atomic, Value} -> Value;
        {aborted, _Reason} -> timeout
    end.

%% Funzione set/6 per scrivere una cella
set(Spreadsheet, Tab, I, J, Value, Timeout) ->
    Fun = fun() ->
        mnesia:write({spreadsheet_data, {Spreadsheet, Tab, I, J}, Value})
    end,
    case mnesia:transaction(Fun, Timeout) of
        {atomic, ok} -> true;
        {aborted, _Reason} -> timeout
    end.

%% Funzione share/2 per gestire le politiche di accesso
share(Spreadsheet, AccessPolicies) ->
    Fun = fun() ->
        % Rimuovi le vecchie politiche
        mnesia:delete({access_policies, Spreadsheet}),
        % Inserisci le nuove politiche
        lists:foreach(fun({Proc, Access}) ->
            mnesia:write({access_policies, {Spreadsheet, Proc, Access}})
        end, AccessPolicies)
    end,
    case mnesia:transaction(Fun) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.
