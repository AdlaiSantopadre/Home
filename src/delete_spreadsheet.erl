-module(delete_spreadsheet).
-export([delete_spreadsheet/1,replace_records/2,read_records/1]).%,delete_owners/1
-include("records.hrl").
read_records(SpreadsheetName)-> 
    mnesia:transaction(fun() ->
        Records = mnesia:match_object(#spreadsheet_data{name = SpreadsheetName, tab = '_', row = '_', col = '_', value = '_'}),
     io:format("Records fetched for deletion: ~p~n", [Records])
    end).
delete_spreadsheet(SpreadsheetName) -> 
    case mnesia:transaction(fun() ->
        Records = mnesia:match_object(#spreadsheet_data{name = SpreadsheetName, tab = '_', row = '_', col = '_', value = '_'}),
        io:format("Records fetched for deletion: ~p~n", [Records]),
        lists:foreach(fun(Record) ->
            io:format("Deleting record: ~p~n", [Record]),
            mnesia:delete_object(Record)
        end, Records),
        Records
    end) of
        {atomic, DeletedRecords} ->
            io:format("Deleted records: ~p~n", [DeletedRecords]),
            ok;
        {aborted, Reason} ->
            io:format("Transaction aborted: ~p~n", [Reason]),
            {error, Reason}
    end.

replace_records(Name, NewRecords) ->
    Fun = fun() ->
        %% Cancellare tutti i record con il campo `name` corrispondente
        io:format("Cancellazione dei record per il nome: ~p~n", [Name]),
        mnesia:delete(spreadsheet_data, Name, write),

        %% Inserire i nuovi record
        io:format("Inserimento dei nuovi record: ~p~n", [NewRecords]),
        lists:foreach(fun(Record) ->
            mnesia:write(Record)
        end, NewRecords)
    end,

    %% Esegui la transazione
    case mnesia:transaction(Fun) of
        {atomic, ok} ->
            io:format("Sostituzione completata con successo per il nome ~p.~n", [Name]);
        {aborted, Reason} ->
            io:format("Errore durante la sostituzione per il nome ~p: ~p~n", [Name, Reason])
    end.
