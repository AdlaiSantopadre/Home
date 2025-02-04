-module(demo_menu).
-export([start/0]).



start() ->
    io:format("~n===========  Setup ====================~n"),
    io:format("1 -> Setup and Start Mnesia(Run Once)~n"),
    io:format("E -> Exit to Shell~n"),
    io:format("======================================~n"),

    
    confirm_setup().

confirm_setup() ->
    case get_setup_confirmation() of
        yes ->
            process_option("1"),
            Option = io:get_line(""),
            process_option(string:trim(Option));
        no ->
            io:format("Skipping Mnesia Setup. Continuing with module distribution and application start.~n"),
            continue()
            % Option = io:get_line(""),
            % process_option(string:trim(Option))
    end.

get_setup_confirmation() ->
    io:format("Do you want to run Setup & Start Mnesia? (y/n): "),
    case string:trim(io:get_line("")) of
        "y" -> yes;
        "n" -> no;
        _ -> io:format("Invalid input. Please enter 'y' or 'n'.~n"), get_setup_confirmation()
    end.
continue() ->
    io:format("~n========  Setup ==========================~n"),
   
    io:format("D -> (Re-Compile and Load Modules~n"),
    io:format("2 -> Start Distributed Application OTP~n"),
    io:format("E -> Exit to Shell~n"),
    io:format("============================================~n"),
    Option = io:get_line(""),
    process_option(string:trim(Option)).

    
%% Processa l'opzione selezionata
process_option("1") ->
    %% Esegui il setup del cluster e aggiorna lo stato
    Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'],
    Dirs = ["C:/Users/campus.uniurb.it/Erlang/Alice@DESKTOPQ2A2FL7_data",
            "C:/Users/campus.uniurb.it/Erlang/Bob@DESKTOPQ2A2FL7_data",
            "C:/Users/campus.uniurb.it/Erlang/Charlie@DESKTOPQ2A2FL7_data"],
    cluster_setup:setup_mnesia(Nodes, Dirs),
    io:format("Mnesia is running on nodes: ~p~n", [mnesia:system_info(running_db_nodes)]),
    
    continue();

process_option("E") ->
    %% Esce dal menu e ritorna alla shell
    io:format("Exiting to shell...~n"),
    ok;



process_option("D") ->

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

       
    
process_option("2") ->
    
    io:format("Starting Application ...~n"),
    Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'],
    cluster_setup:start_application(Nodes),
    
    api_test_menu(); % Passa direttamente al menu API.
    

%% Se l'opzione è invalida
process_option(_) ->
    io:format("Invalid option. Please try again.~n"),
    start().
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Menu API
api_test_menu() ->
    io:format("~n========= API Test Submenu =========~n"),
    io:format("API Test Submenu:~n ~n"),
    io:format("a -> Create new Spreadsheet test_sheet ~n"),
    io:format("b -> Set values in Spreadsheet test_sheet~n"),
    io:format("c -> Get value from Spreadsheet test_sheet~n"),
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
    
    
    SpreadsheetName = test_sheet,
    io:format("Enter number of rows (N) (default: 3): "),
    NInput = string:trim(io:get_line("")),
    N = case string:to_integer(NInput) of {error, _} -> 3; {NInt, _} -> NInt end,

    io:format("Enter number of columns (M) (default: 4): "),
    MInput = string:trim(io:get_line("")),
    M = case string:to_integer(MInput) of {error, _} -> 4; {MInt, _} -> MInt end,

    io:format("Enter number of tabs (K) (default: 2): "),
    KInput = string:trim(io:get_line("")),
    K = case string:to_integer(KInput) of {error, _} -> 2; {TabInt, _} -> TabInt end,

    io:format("Creating spreadsheet '~p' with dimensions (~p, ~p, ~p)...~n", [SpreadsheetName, N, M, K]),
    
    case distributed_spreadsheet:new(SpreadsheetName, N, M, K) of
        {ok, _Pid} -> io:format("Spreadsheet '~p' created successfully.~n", [SpreadsheetName]);
        {error, Reason} -> io:format("Failed to create spreadsheet '~p': ~p~n", [SpreadsheetName, Reason])
    end,

    api_test_menu();
    
execute_test("b") ->
    io:format("Setting some sample values in 'test_sheet' spreadsheet~n"),
    SpreadsheetName = test_sheet,
    % Inserimento batch iniziale di dati
    io:format("Executing batch data insertion...~n"),
    distributed_spreadsheet:set(test_sheet,2,1,1, 24),
    distributed_spreadsheet:set(test_sheet,2,1,2, 3.14),
    distributed_spreadsheet:set(test_sheet,2,1,3, "string"),
    distributed_spreadsheet:set(test_sheet,2,1,4, <<10,20>>),
    distributed_spreadsheet:set(test_sheet,2,2,1,self()),
    distributed_spreadsheet:set(test_sheet,2,2,2, [atom, 32]),
    distributed_spreadsheet:set(test_sheet,2,2,3, fun(X) -> X+1 end),
    distributed_spreadsheet:set(test_sheet,2,2,4, []),
    distributed_spreadsheet:set(test_sheet,2,3,1, 3.14),
    distributed_spreadsheet:set(test_sheet,2,3,2, "Hey, Adi"),
    distributed_spreadsheet:set(test_sheet,2,3,3, atomic),
    distributed_spreadsheet:set(test_sheet,2,3,4, ["cani","gatti"]),
    distributed_spreadsheet:set(test_sheet,2,4,1,"Erlang! #1@rocks"),
    distributed_spreadsheet:set(test_sheet,2,4,2,{cane,gatto,topo}),
    distributed_spreadsheet:set(test_sheet,2,4,3,#{key => value}),
    io:format("Batch data insertion completed.~n"),

    
    
    % Chiede se si vogliono inserire altri dati
    io:format("Do you want to insert another value? (y/n): "),
    case string:trim(io:get_line("")) of
        "y" -> insert_values_loop(SpreadsheetName);
        _ -> api_test_menu()
    end;

    
execute_test("c") ->
    io:format("~n[TEST] Retrieving all sample values set in 'test_sheet'~n"),

    distributed_spreadsheet:get(test_sheet, 2, 1, 1),
    distributed_spreadsheet:get(test_sheet, 2, 1, 2),
    distributed_spreadsheet:get(test_sheet, 2, 1, 3),
    distributed_spreadsheet:get(test_sheet, 2, 1, 4),
    distributed_spreadsheet:get(test_sheet, 2, 2, 1),
    distributed_spreadsheet:get(test_sheet, 2, 2, 2),
    distributed_spreadsheet:get(test_sheet, 2, 2, 3),
    distributed_spreadsheet:get(test_sheet, 2, 2, 4),
    distributed_spreadsheet:get(test_sheet, 2, 3, 1),
    distributed_spreadsheet:get(test_sheet, 2, 3, 2),
    distributed_spreadsheet:get(test_sheet, 2, 3, 3),
    distributed_spreadsheet:get(test_sheet, 2, 3, 4),
    distributed_spreadsheet:get(test_sheet, 2, 4, 1),
    distributed_spreadsheet:get(test_sheet, 2, 4, 2),
    distributed_spreadsheet:get(test_sheet, 2, 4, 3),

    % Chiede se si vuole leggere un valore specifico
    io:format("Do you want to read a specific value? (y/n): "),
    case string:trim(io:get_line("")) of
        "y" -> retrieve_specific_value();
        _ -> api_test_menu()
    end,

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
    
    
%%%%%%%%%%%    % Avvia il loop di inserimento dati
    

insert_values_loop(SpreadsheetName) ->
    io:format("Enter Tab index: "),
    TabInput = string:trim(io:get_line("")),
    Tab = case string:to_integer(TabInput) of {error, _} -> 1; {TabInt, _} -> TabInt end,

    io:format("Enter Row index: "),
    RowInput = string:trim(io:get_line("")),
    Row = case string:to_integer(RowInput) of {error, _} -> 1; {RowInt, _} -> RowInt end,

    io:format("Enter Column index: "),
    ColInput = string:trim(io:get_line("")),
    Col = case string:to_integer(ColInput) of {error, _} -> 1; {ColInt, _} -> ColInt end,

    io:format("Enter Value (string, number, atom, tuple, etc.): "),
    ValueInput = string:trim(io:get_line("")),
    {ok, Value} = parse_value(ValueInput),

    io:format("Setting value in spreadsheet '~p':~n", [SpreadsheetName]),
    io:format("Tab: ~p, Row: ~p, Col: ~p, Value: ~p~n", [Tab, Row, Col, Value]),

    distributed_spreadsheet:set(SpreadsheetName, Tab, Row, Col, Value),

    % Chiede se si vogliono inserire altri dati
    io:format("Do you want to insert another value? (y/n): "),
    case string:trim(io:get_line("")) of
        "y" -> insert_values_loop(SpreadsheetName);
        _ -> api_test_menu()
    end.
    % Funzione per interpretare i valori inseriti dall'utente
parse_value(ValueStr) ->
    case erl_scan:string(ValueStr ++ ".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> %%%io:format("Setting value in spreadsheet '~p':~n", [Term]),
                             {ok, Term};
                _ -> {ok, ValueStr} % Se non è un termine valido, trattalo come stringa
            end;
        _ -> {ok, ValueStr}
    end.
retrieve_specific_value() ->
    io:format("Enter Tab index: "),
    TabInput = string:trim(io:get_line("")),
    Tab = case string:to_integer(TabInput) of {error, _} -> 1; {TabInt, _} -> TabInt end,

    io:format("Enter Row index: "),
    RowInput = string:trim(io:get_line("")),
    Row = case string:to_integer(RowInput) of {error, _} -> 1; {RowInt, _} -> RowInt end,

    io:format("Enter Column index: "),
    ColInput = string:trim(io:get_line("")),
    Col = case string:to_integer(ColInput) of {error, _} -> 1; {ColInt, _} -> ColInt end,

    io:format("Retrieving value from 'test_sheet': Tab: ~p, Row: ~p, Col: ~p~n", [Tab, Row, Col]),

    case distributed_spreadsheet:get(test_sheet, Tab, Row, Col) of
        {ok, Value} -> io:format("Retrieved Value: ~p~n", [Value]);
        {error, Reason} -> io:format("Failed to retrieve value: ~p~n", [Reason])
    end,

    % Chiede se si vogliono leggere altri valori
    io:format("Do you want to read another value? (y/n): "),
    case string:trim(io:get_line("")) of
        "y" -> retrieve_specific_value();
        _ -> api_test_menu()
    end.
