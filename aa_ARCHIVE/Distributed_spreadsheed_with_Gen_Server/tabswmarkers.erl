-module(tabswmarkers).
-export([load_tabs_with_markers/2,read_lines_until_eof/1]).

%% Function to load rows for a single tab
load_rows(File, AccRows) ->
    io:format("AccRows: ~p~n", [AccRows]),
    case io:get_line(File, '') of
        eof -> {lists:reverse(AccRows), eof};
        {error, Reason} -> {error, Reason};
        Line ->
            TrimmedLine = string:trim(Line, both),
            %% Check if it's the end of the current tab or a new "Tab:" marker
            case lists:prefix("EndTab", TrimmedLine) of
                true -> 
                    io:format("Found EndTab marker while reading rows: ~p~n", [TrimmedLine]),
                    {lists:reverse(AccRows), TrimmedLine}; %% Preserve marker for next call
                _ -> 
                        Row = string:tokens(TrimmedLine, ","),
                        load_rows(File, [Row | AccRows])
                        
            end
    end.



%% Function to load all tabs using markers
load_tabs_with_markers(File, AccTabs) ->
    io:format("AccTabs are: ~p~n", [AccTabs]),
    case io:get_line(File, '') of
        eof -> lists:reverse(AccTabs);
        {error, Reason} -> {error, Reason};
        Line ->
            TrimmedLine = string:trim(Line, both),
            io:format("Reading line: ~p~n", [TrimmedLine]),
            
            case lists:prefix("Tab:", TrimmedLine) of
                true ->
                    %% Extract the tab number after "Tab:"
                    Rest = string:trim(lists:nth(2, string:split(TrimmedLine, ":")), both),
                    io:format("Extracted Tab marker: ~p~n", [Rest]),
                    
                    case catch list_to_integer(Rest) of
                        TabNumber when is_integer(TabNumber) ->
                            io:format("Found new Tab marker for tab ~p~n", [TabNumber]),
                            {TabRows, NextLine} = load_rows(File, []),
                            io:format("Loaded rows for tab ~p: ~p~n", [TabNumber, TabRows]),
                            io:format("NextLine: ~p~n", [NextLine]),
                              %% If NextLine is a marker, continue; otherwise, we've reached EOF
                            case NextLine of
                                eof ->
                                io:format("Finally Loaded tabs ~p~n: ~p~n", [TabRows,AccTabs]),
                               
                                Tabs= lists:reverse(AccTabs ++ [TabRows ]),
                                {Tabs};
                                _ -> load_tabs_with_markers(File, AccTabs ++ [TabRows ])
                                
                            end;
                        _ -> {error, invalid_tab_number}
                    end;
                false ->
                    io:format("Unexpected line format: ~p~n", [TrimmedLine]),
                    {error, {unexpected_line_format, TrimmedLine}}
            end
    end.

read_lines_until_eof(File) ->
    case io:get_line(File, '') of
        eof ->
            io:format("Reached end of file~n"),
            done;  % Handle the end of file case
        Line ->
            TrimmedLine = string:trim(Line, both),
            io:format("Read line: ~p~n", [TrimmedLine]),
            read_lines_until_eof(File)  % Continue reading recursively
       
    end.
