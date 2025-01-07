-module(cluster_setup).
-export([init_cluster/0]).
-export([test_init_access_policies/1]).
-include("records.hrl").

init_cluster() ->
    MyGlobalName = list_to_atom("nodo" ++ atom_to_list(node())),
    global:register_name(MyGlobalName, self()),
    io:format("Pid locale ~p registrato globalmente come ~p~n", [self(), MyGlobalName]),
    %% 1) Recupera l'elenco dei nodi del cluster
    Nodes = nodes(),
    %% Recupera il Pid locale per ogni nodo e registra globalmente il nome

    %% Esegue il comando su ciascun nodo
    lists:map(fun(Node) ->
        %% 2) Controlla che siano connessi
        case net_adm:ping(Node) of
            pong ->
                %% 3) Esegui la RPC a node_monitor per recuperare il pid
                case rpc:call(Node, node_monitor, monitor_nodes, []) of
                    {ok, LocalPid} when is_pid(LocalPid) ->
                        %% Crea un atomo unico per il nodo
                        GlobalName = list_to_atom("nodo" ++ atom_to_list(Node)),

                        %% Registra globalmente il LocalPid
                        case global:register_name(GlobalName, LocalPid) of
                            yes ->
                                io:format("Pid locale ~p registrato globalmente come ~p~n", [LocalPid, GlobalName]),
                                {GlobalName, read}; % Tutti i nodi del cluster vengono inseriti con policy read
                            {error, Reason} ->
                                io:format("Fallita la registrazione del Pid locale ~p: ~p~n", [LocalPid, Reason]),
                                {GlobalName, error}
                        end;
                    _ ->
                        io:format("Fallito il recupero del Pid locale dal nodo ~p~n", [Node]),
                        {error, undefined}
                end;
            pang ->
                io:format("Nodo ~p non raggiungibile.~n", [Node]),
                {error, unreachable}
        end
    end, Nodes).

%% 4) crea una funzione  per popolare access_policies.Questa funzione andrà copiata sul genserver distributed_spreadsheet.Verrà chiamata da init/1
%%    -per il  nodo dell' ownerpid dopo esser stata popolata #spreadsheet_info ,
%% aggiungerà ad access_poicies il record SpreadsheetName, [nodo+node(), read]
%% eccetto che per il nodo attuale
%% inizializza la tabella con le access policies per ogni nodo del cluster    
                               
                             
%     %% Inserisce le politiche di accesso nella tabella Mnesia
%     case populate_access_policies(SpreadsheetName, Policies) of
%         {atomic,ok} ->
%             io:format("Access policies initialized for spreadsheet ~p: ~p~n", [SpreadsheetName, Policies]),
%             {ok, Policies};
%         {aborted, Reason} ->
%             io:format("Failed to populate access policies: ~p~n", [Reason]),
%             {error, Reason}
%     end.
% % procura Nodes
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
