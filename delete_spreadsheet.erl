-module(delete_spreadsheet).
-export([delete_spreadsheet/1,delete_owners/1]).
-include("spreadsheet_data.hrl").
-include("spreadsheet_owners.hrl").

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
delete_owners(SpreadsheetName) -> 
    case mnesia:transaction(fun() ->
        Records = mnesia:match_object(#spreadsheet_owners{name = '_', owner = '_'}),
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