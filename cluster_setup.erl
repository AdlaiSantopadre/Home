-module(cluster_setup).

-export([start_cluster/0]).

-export([setup/0,setup_mnesia/2,distribute_modules/2,start_application/1]).%


-include("records.hrl").

%% Funzione principale per configurare il cluster
setup() ->
    %% Ricompila tutti i moduli
    Modules = [distributed_spreadsheet, spreadsheet_supervisor, my_app,
               app_sup, node_monitor, cluster_setup, restart_node],
    lists:foreach(fun(Module) -> compile:file(Module) end, Modules),

    %% Nodi del cluster
    Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'],
    %  Directories di lavoro dei nodi Mnesia
    %  Dirs = ["C:/Users/campus.uniurb.it/Erlang/Alice@DESKTOPQ2A2FL7_data",
    %         "C:/Users/campus.uniurb.it/Erlang/Bob@DESKTOPQ2A2FL7_data",
    %         "C:/Users/campus.uniurb.it/Erlang/Charlie@DESKTOPQ2A2FL7_data"
    %         ],
    
    %% Distribuisci i moduli ai nodi
    distribute_modules(Nodes, Modules),
    
    io:format("Cluster setup completato con successo.~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Funzione per distribuire i moduli
distribute_modules(Nodes, Modules) ->
    lists:foreach(fun(Node) ->
        lists:foreach(fun(Module) ->
            case code:get_object_code(Module) of
                {Module, Binary, FileName} ->
                    %% Carica dinamicamente il modulo sul nodo remoto
                    rpc:call(Node, code, load_binary, [Module, FileName, Binary]),
                    io:format("Modulo ~p distribuito su nodo ~p~n", [Module, Node]);
                _ ->
                    io:format("Modulo ~p non trovato o non compilato.~n", [Module])
            end
        end, Modules)
    end, Nodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Setup iniziale di Mnesia 
setup_mnesia(Nodes, Dirs) ->
    %% Imposta la directory di Mnesia per ogni nodo
    lists:zipwith(fun(Node, Dir) -> 
        rpc:call(Node, application, set_env, [mnesia, dir, Dir])
    end, Nodes, Dirs),

    %% Ferma Mnesia su tutti i nodi se trattasi di ripristino
    lists:foreach(fun(Node) ->
        rpc:call(Node, mnesia, stop, [])
    end, Nodes),

    %% Cancella schema precedente su tutti i nodi se trattasi di ripristino
    lists:foreach(fun(Node) ->
        rpc:call(Node, mnesia, delete_schema, [Nodes])
    end, Nodes),

    %% Crea lo schema sui nodi specificati
    mnesia:create_schema(Nodes),
    create_tables(Nodes).
    
%%% Crea le tabelle e avvia Mnesia sui nodi del cluster
create_tables(Nodes) ->
    
        %%Avvia Mnesia 
        lists:foreach(fun(Node) ->
            rpc:call(Node, mnesia, start, [])
        end, Nodes),

    %% Crea la tabella per i dati del foglio di calcolo con replica su disco
    mnesia:create_table(spreadsheet_data, [
        {attributes, record_info(fields, spreadsheet_data)},
        {type, bag}, 
        {disc_copies, Nodes},
        {index, [tab, row, col]} % Indici per ottimizzare le query
        ]),
    %% Crea la tabella per le politiche di accesso con replica su disco
    mnesia:create_table(access_policies, [
        {attributes, record_info(fields,access_policies)},
        {type, bag}, 
        {disc_copies, Nodes}
    ]),
    
    %% Tabella metadati degli spreadsheet replicata su disco
    mnesia:create_table(spreadsheet_info, [
    {attributes, record_info(fields, spreadsheet_info)},
    {disc_copies, Nodes}]),

    mnesia:wait_for_tables([access_policies,spreadsheet_data,spreadsheet_info], 10000).    
    

%%%%%funzione utilizzata in avvio del nodo monitor_service 

start_cluster() ->

% Registra il nodo di servizio
    MyGlobalName = list_to_atom("nodo" ++ atom_to_list(node())),
    global:register_name(MyGlobalName, self()),
    io:format("Pid locale ~p registrato globalmente come ~p~n", [self(), MyGlobalName]),


    %%  Elenco iniziale dei nodi del cluster
    Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'],
    %% Recupera il Pid locale per ogni nodo e registra globalmente il nome
    lists:foreach(fun(Node) ->
        case net_adm:ping(Node) of
            pong ->
                io:format("Connesso a ~p~n", [Node]),

                %% Usa spawn per avviare il gen_server                
                Pid = spawn(Node, node_monitor, monitor_nodes, []),
                io:format("Monitor avviato su ~p con PID ~p~n", [Node, Pid]);
            pang ->
                io:format("Nodo ~p non raggiungibile.~n", [Node])
        end
    end, Nodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Funzione che  Avvia my_app su tutti i nodi 
%%(esegue con detachment eispetto al runtime)
start_application(Nodes) ->
    lists:foreach(fun(Node) ->
        rpc:call(Node, application, start, [my_app])
        
    end, Nodes).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

