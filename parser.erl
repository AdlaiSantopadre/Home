-module(parser).
-export([read_from_csv/1,read_lines/3,parse_csv_line/2,parse_value/1,tokenize_csv/1]).
-include("records.hrl").
read_from_csv(Filename) ->
    case file:open(Filename, [read]) of
        {ok, IoDevice} ->
            case io:get_line(IoDevice, '') of
                "Spreadsheet Name: " ++ SpreadsheetNameLine ->
                    Name = string:strip(SpreadsheetNameLine, both, $\n),
                    io:format("Name ~p~n",[Name]),
                    %% Salta l'intestazione del CSV
                    io:get_line(IoDevice, ''),
                    case read_lines(IoDevice, [], test) of
                        {ok, Records} ->
                            file:close(IoDevice),
                            {ok, Records};
                        {error, Reason} ->
                            file:close(IoDevice),
                            {error, Reason}
                    end;
                 _ ->
                     file:close(IoDevice),
                     {error, intestazione_scorretta}
            end;
        {error, Reason} -> {error, Reason}
    end.

read_lines(IoDevice, Acc, Name) ->
    case io:get_line(IoDevice, '') of
        eof -> {ok, lists:reverse(Acc)};
        Line ->
            io:format("mostra Line: ~p~n",[Line]),
            case parse_csv_line(Line, Name) of
                {ok, Record} ->
                    read_lines(IoDevice, [Record | Acc], Name);
                {error, Reason} ->
                    %% Logga la riga malformata
                    io:format("Riga malformata: ~p, errore: ~p~n", [Line, Reason]),
                    read_lines(IoDevice, Acc, Name)
            end
    end.
parse_csv_line(Line, Name) ->
    
    case tokenize_csv(Line) of
        {ok, {Tab, Row, Col, ValueStr}} ->
            io:format("valore di ValueString ~p~n",[ValueStr]),
                        
            case parse_value(ValueStr) of
                {ok, ParsedValue} ->
                    {ok, #spreadsheet_data{
                        name = Name,
                        tab = list_to_integer(Tab),
                        row = list_to_integer(Row),
                        col = list_to_integer(Col),
                        value = ParsedValue
                    }};
                {error, Reason} -> {error, {invalid_value, Reason}}
            end;
        {error, Reason} -> {error, Reason}
    end.

tokenize_csv(Line) ->
    case string:split(Line, ",", all) of
        %% Una linea ben formata deve avere almeno 4 valori
        [TabStr, RowStr, ColStr | Rest] ->
            %string:strip(Rest, both, $\n)
            %% Il quarto valore è una lista di ciò che rimane dopo la terza virgola
            ValueStr = lists:foldl(fun(Elem, Acc) ->
                case Acc of
                    "" -> Elem;
                    _ -> Acc ++ "," ++ Elem
                end
            end, "", Rest),
            %% Verifica che i primi tre termini siano interi
            case {list_to_integer(TabStr), list_to_integer(RowStr), string:to_integer(ColStr)} of
                {{ok, Tab}, {ok, Row}, {ok, Col}} ->
                    {ok, {Tab, Row, Col, clean_token(ValueStr)}};
                _ ->
                    {error, invalid_format}
            end;
        _ ->
            {error, invalid_line}
    end.
clean_token(Token) ->
    %% Rimuove spazi e \n dal token
    string:strip(Token, both, $\n).

parse_value(ValueStr) ->
    
    
            try erl_scan:string(ValueStr ++ ".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} ->
                    {ok, Term};
                {error, Reason} ->
                    {error, {invalid_value, Reason}}
            end;
        {error, Reason, _} ->
            {error, {scan_failed, Reason}}
    catch
        _:_ -> {error, invalid_format}
    end.