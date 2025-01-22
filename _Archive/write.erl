-module(write).
-export([write_csv_header/1,test_write_csv/3]).
-include("records.hrl").
write_csv_header(FileName) ->
    case file:open(FileName, [write]) of
        {ok, IoDevice} ->
            file:write(IoDevice, "Tab,Row,Col,Value\n"),
            file:close(IoDevice),
            ok;
        {error, Reason} ->
            io:format("Failed to open file: ~p~n", [Reason]),
            {error, Reason}
    end.
test_write_csv(Filename,SpreadsheetName, Value) ->
    try
        Extension = ".csv",
        File = atom_to_list(Filename) ++ Extension,
        %% Apre il file in modalità scrittura
        file:open(File, [write])
    of
         {ok, IoDevice} ->
            %% Scrive l'intestazione
            io:format(IoDevice, "Spreadsheet Name: ~s~n", [SpreadsheetName]),
            file:write(IoDevice, "Tab,Row,Col,Value\n"),

            %% Scrive i record
                Tab = 1,Row = 2, Col = 3 ,                %% Genera la linea CSV
                    Line = io_lib:format("~p,~p,~p,~p\n", [Tab, Row, Col, Value]),
                    io:format("mostra Line ~p~n",[Line]),
                    file:write(IoDevice, Line),
                
            
            file:close(IoDevice),
            ok
    catch
        _:_ -> {error, write_failed}
    end.
