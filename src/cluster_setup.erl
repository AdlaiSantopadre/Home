-module(cluster_setup).

-export([start_cluster/0]).
-export([setup_mnesia/2,distribute_modules/2,start_application/1]).%


-include("records.hrl").

%%%%%funzione utilizzata in avvio del nodo monitor_service 

start_cluster() ->

%%avvia node_monitor locale sul nodo Monitor_service
    case node_monitor:start_link() of
        {ok, MonitorPid} ->
            % Registra globalmente il PID del node_monitor
            MyGlobalName = 'nodeMonitor_service@DESKTOPQ2A2FL7',
            global:register_name(MyGlobalName, MonitorPid),
            io:format("Pid ~p del node_monitor registrato globalmente come ~p~n", [MonitorPid, MyGlobalName]);
        {error, Reason} ->
            io:format("Errore nell'avvio di node_monitor: ~p~n", [Reason])
    end,


%%  Elenco iniziale dei nodi del cluster
    Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'],
    %% Recupera il Pid di ogni nodo e registra globalmente il nome
    lists:foreach(fun(Node) ->
        case net_adm:ping(Node) of
            pong ->
                io:format("Connesso a ~p~n", [Node]),
                %% Usa spawn per avviare il (gen_server ) node_monitor               
                Pid = spawn(Node, node_monitor, monitor_nodes, []),
                io:format("Monitor avviato su ~p con PID ~p~n", [Node, Pid]);
            pang ->
                io:format("Nodo ~p non raggiungibile.~n", [Node])
        end
    end, Nodes),

%% Distribuisci e carica i moduli nei nodi
    Modules = [distributed_spreadsheet, spreadsheet_supervisor, my_app,app_sup,  %% moduli di APPLICATION OTP
              node_monitor, cluster_setup, restart_node,demo_menu],              %% moduli del node_monitor e per la gestione del cluster
      distribute_modules(Nodes, Modules),
    
      io:format("Cluster setup completato con successo.~n").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% %% Funzione principale per configurare il cluster
% setup() ->
%     %% Ricompila tutti i moduli
%     Modules = [distributed_spreadsheet, spreadsheet_supervisor, my_app,
%                app_sup, node_monitor, cluster_setup, restart_node],
%     %%lists:foreach(fun(Module) -> compile:file(Module,[{outdir,"C:\Users\campus.uniurb.it\Erlang\ebin"}]) end, Modules),

%     %% Nodi del cluster
%     Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'],
    
%     
% %% code:get_object_code(module) permette di verificare l'origine da cui e' stato caricato un modulo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Funzione per distribuire i moduli
distribute_modules(Nodes, Modules) ->
    lists:foreach(fun(Node) ->
        lists:foreach(fun(Module) ->
            case code:get_object_code(Module) of
                {Module, Binary, FileName} ->
                    %% Carica dinamicamente il modulo sul nodo remoto
                    rpc:call(Node, code, load_binary, [Module, FileName, Binary]),
                    io:format("Modulo ~p distribuito su nodo ~p~n", [Module, Node]); %%VERBOSE
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
    


%%Funzione che  Avvia my_app su tutti i nodi 
%%(esegue con detachment rispetto al runtime)
start_application(Nodes) ->
    lists:foreach(fun(Node) ->
        rpc:call(Node, application, start, [my_app])
        
    end, Nodes).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

