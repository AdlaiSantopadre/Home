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
    io:format("S -> Start Distributed Application OTP~n"),
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

       
    
process_option("S") ->
    
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
    io:format("new -> Create new Spreadsheet test_sheet ~n"),
    io:format("set -> Set values in Spreadsheet test_sheet~n"),
    io:format("get -> Get value from Spreadsheet test_sheet~n"),
    io:format("info -> Retrieve Spreadsheet info~n"),
    io:format("to -> Export Spreadsheet To CSV~n"),
    io:format("from -> Import Spreadsheet From CSV~n"),
    io:format("share -> Share Spreadsheet~n~n"),
    
    io:format("E -> Exit API Test Menu~n"),
    io:format("====================================~n"),    
    io:format("Select an API test option: "),
    
    Option = io:get_line(""),
    execute_test(string:trim(Option)).

execute_test("new") ->
    
    
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
    
execute_test("set") ->
    io:format("Setting some sample values in 'test_sheet' spreadsheet~n"),
    SpreadsheetName = test_sheet,
    % Inserimento batch iniziale di dati
    io:format("Executing batch data insertion...~n"),
    distributed_spreadsheet:set(test_sheet,2,1,1, 24),
    distributed_spreadsheet:set(test_sheet,2,1,2, 3.14),
    distributed_spreadsheet:set(test_sheet,2,1,3, "string"),
    distributed_spreadsheet:set(test_sheet,2,1,4, <<10,20>>),
    distributed_spreadsheet:set(test_sheet,2,2,1, self()),
    distributed_spreadsheet:set(test_sheet,2,2,2, [atom, 32]),
    distributed_spreadsheet:set(test_sheet,2,2,3, fun(X) -> X+1 end),
    distributed_spreadsheet:set(test_sheet,2,2,4, {cane,gatto,topo}),
    distributed_spreadsheet:set(test_sheet,2,3,1, #{key => value}),
    distributed_spreadsheet:set(test_sheet,2,3,2, "Hey, Adi"),
    distributed_spreadsheet:set(test_sheet,2,3,3, atomic),
    distributed_spreadsheet:set(test_sheet,2,3,4, ["cani","gatti"]),
    
    

    io:format("Batch data insertion completed.~n"),

    
    
    % Chiede se si vogliono inserire altri dati
    io:format("Do you want to insert another value? (y/n): "),
    case string:trim(io:get_line("")) of
        "y" -> insert_values_loop(SpreadsheetName);
        _ -> api_test_menu()
    end;

    
execute_test("get") ->
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


    % Chiede se si vuole leggere un valore specifico
    io:format("Do you want to read a specific value? (y/n): "),
    case string:trim(io:get_line("")) of
        "y" -> retrieve_specific_value(),ok;
        _ -> api_test_menu()
    end,

    api_test_menu();
    
execute_test("info") ->
    io:format("Retrieving info of 'test_sheet'~n"),
    case distributed_spreadsheet:info(test_sheet) of
        {ok, Info} -> print_map(Info);
        {error, Reason} -> io:format("Failed to retrieve info: ~p~n", [Reason])
    end,
    api_test_menu();
    
execute_test("to") ->
    io:format("Exporting Spreadsheet test_sheet to CSV file 'demo_sheet.csv'~n"),
    distributed_spreadsheet:to_csv(test_sheet, demo_sheet),
    io:format("Spreadsheet exported successfully.~n"),
    api_test_menu();
    
execute_test("from") ->
    io:format("Importing 'demo_sheet.csv' back into a spreadsheet~n"),
    distributed_spreadsheet:from_csv("demo_sheet.csv"),
    io:format("Spreadsheet imported successfully.~n"),
    api_test_menu();
    
execute_test("share") ->
    io:format("~n[TEST] Configuring access policies for 'test_sheet'~n"),
    AccessPolicies = collect_access_policies(),
    io:format("Applying access policies: ~p~n", [AccessPolicies]),
    distributed_spreadsheet:share(test_sheet, AccessPolicies),
    
    api_test_menu();
    
execute_test("E") ->
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

    distributed_spreadsheet:get(test_sheet, Tab, Row, Col),

    % Chiede se si vogliono leggere altri valori
    io:format("Do you want to read another value? (y/n): "),
    case string:trim(io:get_line("")) of
        "y" -> retrieve_specific_value();
        _ -> api_test_menu()
    end.
print_map(Map) when is_map(Map) ->
    maps:foreach(fun(Key, Value) ->
        io:format("~p: ~p~n", [Key, Value])
    end, Map).
collect_access_policies() ->
    PossibleNodes = ['nodeAlice@DESKTOPQ2A2FL7', 'nodeBob@DESKTOPQ2A2FL7', 'nodeCharlie@DESKTOPQ2A2FL7'],
    io:format("Available nodes: ~p~n", [PossibleNodes]),
    io:format("Enter node name from the list above (or empty to finish): "),
    NodeInput = string:trim(io:get_line("")),
    case lists:member(list_to_atom(NodeInput), PossibleNodes) of
        true ->
            io:format("Enter access policy (read/write): "),
            PolicyInput = string:trim(io:get_line("")),
            case PolicyInput of
                "read" -> [{list_to_atom(NodeInput), read} | collect_access_policies()];
                "write" -> [{list_to_atom(NodeInput), write} | collect_access_policies()];
                _ ->
                    io:format("Invalid policy. Please enter 'read' or 'write'.~n"),
                    collect_access_policies()
            end;
        false when NodeInput =:= "" -> [];
        false ->
            io:format("Invalid node name. Please select from the available list.~n"),
            collect_access_policies()
    end.