-module(demo_menu).
-export([start/0]).

%% Stato iniziale del menu
-define(STATE, #{setup_done => false}).

start() ->
        start(?STATE).
start(State) ->
    io:format("1 -> Setup and Start Mnesia~n"),
    io:format("2 -> (optional) Recompile  Modules~n"),
    io:format("3 -> Start Application OTP~n"),
    io:format("7 -> Exit to Shell~n"),
    io:format("4 -> API Test Menu~n"),
    io:format("Select an option: "),
    Option = io:get_line(""),
    process_option(string:trim(Option), State).

%% Processa l'opzione selezionata
process_option("1", State) ->
    %% Esegui il setup del cluster e aggiorna lo stato
    Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'],
    Dirs = ["C:/Users/campus.uniurb.it/Erlang/Alice@DESKTOPQ2A2FL7_data",
            "C:/Users/campus.uniurb.it/Erlang/Bob@DESKTOPQ2A2FL7_data",
            "C:/Users/campus.uniurb.it/Erlang/Charlie@DESKTOPQ2A2FL7_data"],
    cluster_setup:setup_mnesia(Nodes, Dirs),
    io:format("Mnesia is running on nodes: ~p~n", [mnesia:system_info(running_db_nodes)]),
     NewState = State#{setup_done => true}, %% Stato aggiornato,
    start(NewState);
    
process_option("2", State) ->

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
    start(State);

       
    
process_option("3", State) ->
    %% Estrai i valori dalla mappa
    SetupDone = maps:get(setup_done, State),
    io:format("setupDone ~p~n",[SetupDone]),
    case SetupDone of
        true   ->
            %% Avvia l'applicazione e Observer
            Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'],
            cluster_setup:start_application(Nodes),            
            start(State);
        _ ->
            io:format("Please complete setup (1)  first.~n"),
            start(State)
    end;
    
process_option("4", State) ->
    %% Estrai i valori dalla mappa
    SetupDone = maps:get(setup_done, State),
    
    case SetupDone of
        true   ->
            %% Mostra il menu delle API
            api_test_menu(),
            start(State);
        _ ->
            io:format("Please complete setup (1) and distribute modules (2) first.~n"),
            start(State)
    end;
process_option("7", _State) ->
    %% Esce dal menu e ritorna alla shell
    io:format("Exiting to shell...~n"),
    ok;
%% Se l'opzione è invalida
process_option(_, State) ->
    io:format("Invalid option. Please try again.~n"),
    start(State).

%% Menu API
api_test_menu() ->
    io:format("API Test Submenu:~n"),
    io:format("a -> Test API Function A~n"),
    io:format("b -> Test API Function B~n"),
    
    io:format("Select an API test option: "),
    Option = io:get_line(""),
    case string:trim(Option) of
        "a" -> io:format("Testing API Function A~n");  % Replace with actual function call
        "b" -> io:format("Testing API Function B~n");  % Replace with actual function call
        _ -> io:format("Invalid API test option.~n")
    end.
