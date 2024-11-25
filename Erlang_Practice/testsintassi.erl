-module(testsintassi).
-include("spreadsheet_data.hrl"). 
-export([new/4,generate_records/4]).
generate_records(Name, N, M, K) ->
    % Genera una lista di tutti i record #spreadsheet_data
    [#spreadsheet_data{name = Name, tab = Tab, row = Row, col = Col, value = undef}
     || Tab <- lists:seq(1, K),
        Row <- lists:seq(1, N),
        Col <- lists:seq(1, M)].
init(Name, N, M, K) ->
    % Genera tutti i record per lo spreadsheet
    Records = generate_records(Name, N, M, K),
    % Inserisce tutti i record con una transazione Mnesia
    mnesia:transaction(fun() ->
        lists:foreach(fun(Record) -> mnesia:write(Record) end, Records)
    end),
    {ok, Name}.
new(SpreadsheetName, N, M, K) ->
    mnesia:transaction(fun() ->
        case mnesia:read({spreadsheet_owners, SpreadsheetName}) of
            [] -> 
                % Inserisce l'owner dello spreadsheet
                %PROSSIMAMENTE REGISTRARE IL PID E SCRIVERE IL global name
                mnesia:write({spreadsheet_owners, SpreadsheetName, self()}),
                init(SpreadsheetName, N, M, K);
            _ -> {error, already_exists}
        end
    end).