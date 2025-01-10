%%Questo modulo prenderà il posto di Mnesia_setup


-module(cluster_setup).
-export([start_cluster/0]).
-export([setup/0, distribute_modules/2]).

%-export([test_init_access_policies/1]).
-include("records.hrl").
%% Funzione principale per configurare il cluster
setup() ->
    %% Ricompila tutti i moduli
    Modules = [distributed_spreadsheet, spreadsheet_supervisor, my_app,
               app_sup, node_monitor, mnesia_setup, cluster_setup, restart_node],
    lists:foreach(fun(Module) -> compile:file(Module) end, Modules),

    %% Nodi del cluster
    Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'],

    %% Distribuisci i moduli ai nodi
    distribute_modules(Nodes, Modules),

    
    io:format("Cluster setup completato con successo.~n").

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
    
                             
                             
%%% Inserisce le politiche di accesso nella tabella Mnesia
%
test_init_access_policies(SpreadsheetName) ->

    mnesia:transaction(fun() ->
        %% Rimuovi le politiche esistenti per lo spreadsheet
        mnesia:delete({access_policies, SpreadsheetName}),
        %% Inserisci le nuove politiche
        Nodes=nodes(),
        lists:foreach(fun(Node) ->
            Record = #access_policies{name = SpreadsheetName, proc = list_to_atom("nodo" ++ atom_to_list(Node)), access = read},
            io:format("Inserting access policy: ~p~n", [Record]),
            mnesia:write(Record)
        end, Nodes),
        Node=node(),
        Record = #access_policies{name = SpreadsheetName, proc = list_to_atom("nodo" ++ atom_to_list(Node)), access = write},
            io:format("Inserting access policy: ~p~n", [Record]),
            mnesia:write(Record),
        ok
   end).
