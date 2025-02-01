-module(demo_menu).
-export([start/0]).



start() ->
    io:format("~n=========  Setup =========~n"),
    io:format("1 -> Setup and Start Mnesia(Run Once)~n"),
    io:format("2 -> Distribute and Compile Modules~n"),
    io:format("3 -> Start Application and Observer~n"),
    io:format("e -> Exit to Shell~n"),
    io:format("====================================~n"),

    
    confirm_setup().

confirm_setup() ->
    case get_setup_confirmation() of
        yes ->
            process_option("1"),
            Option = io:get_line(""),
            process_option(string:trim(Option));
        no ->
            io:format("Skipping Mnesia Setup. Continuing with module distribution and application start.~n"),
            Option = io:get_line(""),
            process_option(string:trim(Option))
    end.

get_setup_confirmation() ->
    io:format("Do you want to run Setup & Start Mnesia? (y/n): "),
    case string:trim(io:get_line("")) of
        "y" -> yes;
        "n" -> no;
        _ -> io:format("Invalid input. Please enter 'y' or 'n'.~n"), get_setup_confirmation()
    end.


    
%% Processa l'opzione selezionata
process_option("1") ->
    %% Esegui il setup del cluster e aggiorna lo stato
    Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'],
    Dirs = ["C:/Users/campus.uniurb.it/Erlang/Alice@DESKTOPQ2A2FL7_data",
            "C:/Users/campus.uniurb.it/Erlang/Bob@DESKTOPQ2A2FL7_data",
            "C:/Users/campus.uniurb.it/Erlang/Charlie@DESKTOPQ2A2FL7_data"],
    cluster_setup:setup_mnesia(Nodes, Dirs),
    io:format("Mnesia is running on nodes: ~p~n", [mnesia:system_info(running_db_nodes)]),
    
    start();
    
process_option("2") ->

    %% Specifica la directory di output per i file compilati
    %% Specifica le directory
    SrcDir = "C:/Users/campus.uniurb.it/Erlang/src", % <-- Percorso dei file sorgenti
    OutDir = "C:/Users/campus.uniurb.it/Erlang/ebin", % <-- Directory di output
    %% Lista dei nodi su cui caricare il codice compilato  
    Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7', 'Monitor_service@DESKTOPQ2A2FL7'],

    %% Lista dei moduli da compilare e distribuire
    Modules = [distributed_spreadsheet, spreadsheet_supervisor, my_app, app_sup, node_monitor, cluster_setup, restart_node],

    %% Compila e salva i moduli in `ebin/`, poi caricali in memoria
    lists:foreach(fun(Module) ->
        SrcFile = filename:join(SrcDir, atom_to_list(Module) ++ ".erl"),
        %% Se il modulo è già caricato, lo scarica
        case code:is_loaded(Module) of
            {file, _} -> 
                io:format("Purging old version of ~p~n", [Module]),
                code:purge(Module),
                code:delete(Module);
            false -> ok
        end,
        case compile:file(SrcFile, [{outdir, OutDir}]) of
            {ok, Module} ->
                io:format("Modulo ~p compilato con successo.~n", [Module]),
                code:load_abs(filename:join(OutDir, atom_to_list(Module)));
            Error ->
                io:format("Errore nella compilazione di ~p: ~p~n", [Module, Error])
        end
    end, Modules),

    %% Distribuisci i moduli ai nodi
    cluster_setup:distribute_modules(Nodes,Modules),

    io:format("Modules compiled and distributed successfully.~n"),
    start();

       
    
process_option("3") ->
    
    io:format("Starting Application and Observer...~n"),
    Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'],
    cluster_setup:start_application(Nodes),
    observer:start(),
    api_test_menu(); % Passa direttamente al menu API.
    

process_option("e") ->
    %% Esce dal menu e ritorna alla shell
    io:format("Exiting to shell...~n"),
    ok;
%% Se l'opzione è invalida
process_option(_) ->
    io:format("Invalid option. Please try again.~n"),
    start().
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Menu API
api_test_menu() ->
    io:format("~n========= API Test Submenu =========~n"),
    io:format("API Test Submenu:~n"),
    io:format("a -> Create new Spreadsheet~n"),
    io:format("b -> Set value in Spreadsheet~n"),
    io:format("c -> Get value from Spreadsheet~n"),
    io:format("d -> Retrieve Spreadsheet info~n"),
    io:format("e -> Export Spreadsheet to CSV~n"),
    io:format("f -> Import Spreadsheet from CSV~n"),
    io:format("g -> Share Spreadsheet~n~n"),
    io:format("h -> Export Spreadsheet to CSV~n"),
    io:format("i -> Import Spreadsheet from CSV~n"),
    io:format("j -> Exit API Test Menu~n"),
    io:format("====================================~n"),    
    io:format("Select an API test option: "),
    
    Option = io:get_line(""),
    execute_test(string:trim(Option)).

execute_test("a") ->
    io:format("Creating a new spreadsheet 'test_sheet'~n"),
    case distributed_spreadsheet:new(test_sheet) of
        {ok, _Pid} -> io:format("Spreadsheet created successfully.~n");
        {error, Reason} -> io:format("Failed to create spreadsheet: ~p~n", [Reason])
    end,
    api_test_menu();
    
execute_test("b") ->
    io:format("~n[TEST] Setting different data types in 'test_sheet'~n"),
    
    % Integer
    distributed_spreadsheet:set(test_sheet, 1, 1, 1, 42),

    % Float
    distributed_spreadsheet:set(test_sheet, 1, 1, 2, 3.14),

    % Atom
    distributed_spreadsheet:set(test_sheet, 1, 1, 3, test_atom),

    % List (String)
    distributed_spreadsheet:set(test_sheet, 1, 1, 4, "Hello World"),

    % Tuple
    distributed_spreadsheet:set(test_sheet, 1, 2, 1, {tuple, example}),

    % Map
    distributed_spreadsheet:set(test_sheet, 1, 2, 2, #{key => value}),

    % Binary
    distributed_spreadsheet:set(test_sheet, 1, 2, 3, <<1,2,3>>),

    % PID (self as example)
    distributed_spreadsheet:set(test_sheet, 1, 2, 4, self()),

    api_test_menu();
    
execute_test("c") ->
    io:format("~n[TEST] Retrieving all values set in 'test_sheet'~n"),

    distributed_spreadsheet:get(test_sheet, 1, 1, 1),
    distributed_spreadsheet:get(test_sheet, 1, 1, 2),
    distributed_spreadsheet:get(test_sheet, 1, 1, 3),
    distributed_spreadsheet:get(test_sheet, 1, 1, 4),
    distributed_spreadsheet:get(test_sheet, 1, 2, 1),
    distributed_spreadsheet:get(test_sheet, 1, 2, 2),
    distributed_spreadsheet:get(test_sheet, 1, 2, 3),
    distributed_spreadsheet:get(test_sheet, 1, 2, 4),

    api_test_menu();
    
execute_test("d") ->
    io:format("Retrieving info of 'test_sheet'~n"),
    case distributed_spreadsheet:info(test_sheet) of
        {ok, Info} -> io:format("Spreadsheet info: ~p~n", [Info]);
        {error, Reason} -> io:format("Failed to retrieve info: ~p~n", [Reason])
    end,
    api_test_menu();
    
execute_test("e") ->
    io:format("Exporting 'test_sheet' to CSV file 'test_sheet.csv'~n"),
    distributed_spreadsheet:to_csv(test_sheet, "test_sheet.csv"),
    io:format("Spreadsheet exported successfully.~n"),
    api_test_menu();
    
execute_test("f") ->
    io:format("Importing 'test_sheet.csv' back into a spreadsheet~n"),
    distributed_spreadsheet:from_csv("test_sheet.csv"),
    io:format("Spreadsheet imported successfully.~n"),
    api_test_menu();
    
execute_test("g") ->
    io:format("~n[TEST] Sharing 'test_sheet' with changing policies ~n"),
    
    distributed_spreadsheet:share(test_sheet, 
        [{'nodeAlice@DESKTOPQ2A2FL7', read}, 
         {'nodeBob@DESKTOPQ2A2FL7', read}, 
         {'nodeCharlie@DESKTOPQ2A2FL7', read}]),

    distributed_spreadsheet:share(test_sheet, 
        [{'nodeAlice@DESKTOPQ2A2FL7', write}, 
         {'nodeBob@DESKTOPQ2A2FL7', read}]),

    distributed_spreadsheet:share(test_sheet, 
        [{'nodeAlice@DESKTOPQ2A2FL7', write}, 
         {'nodeBob@DESKTOPQ2A2FL7', read}, 
         {'nodeCharlie@DESKTOPQ2A2FL7', write}]),

    api_test_menu();

execute_test("h") ->
    io:format("~n[TEST] Exporting 'test_sheet' to CSV file 'test_sheet.csv'~n"),
    distributed_spreadsheet:to_csv(test_sheet, "test_sheet.csv"),
    api_test_menu();
    
execute_test("i") ->
    io:format("~n[TEST] Importing 'test_sheet.csv' back into a spreadsheet~n"),
    distributed_spreadsheet:from_csv("test_sheet.csv"),
    api_test_menu();    
    
execute_test("j") ->
    io:format("Invalid API test option. Please try again.~n"),
    
    %% Esce dal menu e ritorna alla shell
    io:format("Exiting to shell...~n"),
    ok;
execute_test(_) ->
    io:format("Invalid API test option. Please try again.~n"),
    
    %% Esce dal menu e ritorna alla shell
    api_test_menu().