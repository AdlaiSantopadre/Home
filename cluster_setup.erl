%%Questo modulo prenderà il posto di Mnesia_setup


-module(cluster_setup).
-export([start_cluster/0]).

-export([test_init_access_policies/1]).
-include("records.hrl").

start_cluster() ->
    % MyGlobalName = list_to_atom("nodo" ++ atom_to_list(node())),
    % global:register_name(MyGlobalName, self()),
    % io:format("Pid locale ~p registrato globalmente come ~p~n", [self(), MyGlobalName]),
%IL COMMENTO ESCLUDE IL NODO DALLA REGISTRAZIONE
    %node_monitor:monitor_nodes(), 

    %% 1) Recupera l'elenco dei nodi del cluster
    Nodes = nodes(),
    %% Recupera il Pid locale per ogni nodo e registra globalmente il nome
    lists:foreach(fun(Node) ->
        case net_adm:ping(Node) of
            pong ->
                io:format("Connesso a ~p~n", [Node]),

                %% Usa spawn per avviare il gen_server                
                Pid = spawn(Node, node_monitor, start_link, []),
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
