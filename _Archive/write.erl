-module(write).
-export([write_csv_header/1]).
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
