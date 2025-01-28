-module(demo_menu).
-export([start/0]).

start() ->
    io:format("1 -> Setup Cluster and Start Mnesia~n"),
    io:format("2 -> Start Application and Observer~n"),
    io:format("3 -> API Test Menu~n"),
    io:format("Select an option: "),
    Option = io:get_line(""),
    process_option(Option).

process_option("1\n") ->
    Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'],
    Dirs = ["C:/Users/campus.uniurb.it/Erlang/Alice@DESKTOPQ2A2FL7_data",
            "C:/Users/campus.uniurb.it/Erlang/Bob@DESKTOPQ2A2FL7_data",
            "C:/Users/campus.uniurb.it/Erlang/Charlie@DESKTOPQ2A2FL7_data"],
    cluster_setup:setup_mnesia(Nodes, Dirs),
    io:format("Mnesia is running on nodes: ~p~n", [mnesia:system_info(running_db_nodes)]),
    start();
process_option("2\n") ->
    Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'],
    cluster_setup:start_application(Nodes),
    observer:start(),
    start();
process_option("3\n") ->
    api_test_menu(),
    start();
process_option(_) ->
    io:format("Invalid option. Please try again.~n"),
    start().

api_test_menu() ->
    io:format("API Test Submenu:~n"),
    io:format("a -> Test API Function A~n"),
    io:format("b -> Test API Function B~n"),
    io:format("Select an API test option: "),
    Option = io:get_line(""),
    case Option of
        "a\n" -> io:format("Testing API Function A~n");  % Replace with actual function call
        "b\n" -> io:format("Testing API Function B~n");  % Replace with actual function call
        _ -> io:format("Invalid API test option.~n")
    end.
