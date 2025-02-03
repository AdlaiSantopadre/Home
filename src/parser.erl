-module(parser).
-export([read_from_csv/1, read_lines/3, parse_csv_line/2,
 parse_value/1, tokenize_csv/1,handle_special_cases/1]).
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
        eof ->
            {ok, lists:reverse(Acc)};
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
        {ok, {Tab, Row, Col , ValueStr}} -> %??
            case parse_value(ValueStr) of
                {ok, ParsedValue} ->
                    {ok, #spreadsheet_data{
                        name = list_to_atom(Name),
                        tab = Tab,
                        row = Row,
                        col = Col,
                        value = ParsedValue
                    }};
                {error, Reason} ->
                    {error, {invalid_value, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.    
tokenize_csv(Line) ->
    case string:split(Line, ",", all) of
        %% Una linea ben formata deve avere almeno 4 valori
        [TabStr, RowStr, ColStr | Rest] ->
            %% Il quarto valore è una lista di ciò che rimane dopo la terza virgola
            ValueStr = lists:foldl(
                fun(Elem, Acc) ->
                    case Acc of
                        "" -> Elem;
                        _ -> Acc ++ "," ++ Elem
                    end
                end,
                "",
                Rest
            ),

            %% Verifica che i primi tre termini siano interi
            case
                {
                    try_list_to_integer(TabStr),
                    try_list_to_integer(RowStr),
                    try_list_to_integer(ColStr)
                }
            of
                {{ok, Tab}, {ok, Row}, {ok, Col}} ->
                    {ok, {Tab, Row, Col, clean_token(ValueStr)}};
                {{error, tab_error}, _, _} ->
                    {error, {invalid_format, "Invalid Tab value"}};
                {_, {error, row_error}, _} ->
                    {error, {invalid_format, "Invalid Row value"}};
                {_, _, {error, col_error}} ->
                    {error, {invalid_format, "Invalid Col value"}}
            end;
        _ ->
            {error, invalid_line}
    end.
clean_token(Token) ->
    %% Rimuove spazi e \n dal token
    string:strip(Token, both, $\n).
try_list_to_integer(Value) ->
    try
        {ok, list_to_integer(Value)}
    catch
        _:_ -> {error, invalid_integer}
    end.

%%%%%%Funzione Helper parse_value/1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_value(ValueStr) ->
     io:format("ValueStr now:~p,~n" ,[ValueStr]),   
    case erl_scan:string(ValueStr ++ ".") of
        {ok, Tokens, _} ->
            io:format("Tokens now:~p,~n" ,[Tokens]),
            case erl_parse:parse_term(Tokens) of
                {ok, Term} ->
                    case is_valid_type(Term) of
                        true -> {ok, Term};
                        false -> handle_special_cases(ValueStr)
                    end;
                _ -> handle_special_cases(ValueStr)
            end;
        _ -> handle_special_cases(ValueStr)
    end.
is_valid_type(Value) when is_integer(Value); is_float(Value); is_atom(Value);
                           is_list(Value); is_tuple(Value); is_map(Value);
                           is_binary(Value) -> true;
is_valid_type(_) -> false.
handle_special_cases(ValueStr) ->
     
    case try_convert_to_pid(ValueStr)  of
       {ok, Pid} when is_pid(Pid) -> {ok,Pid};
       {error, ValueStr} -> {ok,ValueStr}
    end.
try_convert_to_pid(ValueStr) when is_list(ValueStr) ->
    try
        {ok, list_to_pid(ValueStr)}
    catch        
        _:_ -> {error, ValueStr}
    end.


 



