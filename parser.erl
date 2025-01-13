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
                    case read_lines(IoDevice, [], Name) of
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
        {ok, [Tab, Row, Col, ValueStr]} ->
            case parse_value(ValueStr) of
                {ok, Value} ->
                    {ok, #spreadsheet_data{
                        name = list_to_atom(Name),
                        tab = list_to_integer(binary_to_list(Tab)),
                        row = list_to_integer(binary_to_list(Row)),
                        col = list_to_integer(binary_to_list(Col)),
                        value = Value
                    }};
                {error, Reason} -> {error, {invalid_value, Reason}}
            end;
        {error, Reason} -> {error, Reason}
    end.

tokenize_csv(Line) ->
    tokenize_csv(Line, [], false, []).

tokenize_csv([], Acc, _InQuotes, CurrentToken) ->
    %% Aggiungi l'ultimo token alla lista rimuovendo eventuali \n o spazi
    {ok, lists:reverse([list_to_binary(string:strip(CurrentToken, both, $\n)) | Acc])};
tokenize_csv([$\" | Rest], Acc, false, CurrentToken) ->
    %% Inizio di una stringa racchiusa tra virgolette
    tokenize_csv(Rest, Acc, true, CurrentToken);
tokenize_csv([$\" | Rest], Acc, true, CurrentToken) ->
    %% Fine di una stringa racchiusa tra virgolette
    tokenize_csv(Rest, Acc, false, CurrentToken);
tokenize_csv([$, | Rest], Acc, false, CurrentToken) ->
    %% Separatore CSV, aggiungi il token corrente alla lista
    tokenize_csv(Rest, [list_to_binary(CurrentToken) | Acc], false, []);
tokenize_csv([Char | Rest], Acc, InQuotes, CurrentToken) ->
    %% Accumula caratteri nel token corrente
    tokenize_csv(Rest, Acc, InQuotes, CurrentToken ++ [Char]).


parse_value(ValueString) ->
    %% Rimuovi eventuali virgolette esterne
    Stripped = binary_to_list(ValueString),
    case Stripped of
        "undef" -> {ok, undef};
        _ ->
            %% Prova a interpretare come intero
            case string:to_integer(Stripped) of
                {ok, Int} -> {ok, Int};
                _ ->
                    %% Prova a interpretare come float
                    case string:to_float(Stripped) of
                        {ok, Float} -> {ok, Float};
                        _ ->
                            %% Tratta il valore come stringa o termine Erlang
                            try erl_scan:string(Stripped ++ ".") of
                                {ok, Tokens, _} ->
                                    case erl_parse:parse_term(Tokens) of
                                        {ok, Term} -> {ok, Term};
                                        _ -> {ok, Stripped}
                                    end;
                                _ -> {ok, Stripped}
                            catch _:_ -> {ok, Stripped}
                            end
                    end
            end
    end.
