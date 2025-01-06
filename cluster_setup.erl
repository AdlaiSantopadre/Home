-module(cluster_setup).

-include("records.hrl").

%% 1) recupera l' elenco dei nodi del cluster
%% 2) controlla che sono connessi
%% 3) esegui la RPC a monitor_node per tutti i nodi, per avviarne la registrazione globale come nodo+node()
%% 4) crea una funzione  per popolare access_policies.Questa funzione andrà copiata sul genserver distributed_spreadsheet.Verrà chiamata da init/1
%%    -per il  nodo dell' ownerpid dopo esser stata popolata #spreadsheet_info ,
%% aggiungerà ad access_poicies il record SpreadsheetName, [nodo+node(), read]
%% eccetto che per il nodo attuale
%% inizializza la tabella con le access policies per ogni nodo del cluster
init_cluster_policies(Nodes) ->
    %% Recupera il Pid locale per ogni nodo e registra globalmente il nome
    (2) -> pong
    (Policies = )
                lists:map(fun(Node) ->
        %% Esegue il comando su ciascun nodo
    (3)    case rpc:call(Node, parametri per ottenere pid da monitor_node) of
            LocalPid when is_pid(LocalPid) ->
            
            %% Crea un atomo unico per il nodo
            GlobalName = list_to_atom("nodo" ++ atom_to_list(Node)),
            %% Registra globalmente il LocalPid
            case global:register_name(GlobalName, LocalPid) of
                yes ->
                    io:format("Pid locale  ~p registered globally as ~p~n", [LocalPid, GlobalName]),
                    {GlobalName, read}; %tutti i nodo del cluster vengono inseriti con policy read
                no ->
                    io:format("Failed to register LocalPid ~p: ~n", [MasterPid]),
                    {GlobalName,error}
            end;
            undefined ->
                io:format("Failed to retrieve LocalPid from node ~p~n", [Node]),
                {error, undefined}
        end
    end, Nodes),
    
                               
    (4)                          
    %% Inserisce le politiche di accesso nella tabella Mnesia
    case populate_access_policies(SpreadsheetName, Policies) of
        {atomic,ok} ->
            io:format("Access policies initialized for spreadsheet ~p: ~p~n", [SpreadsheetName, Policies]),
            {ok, Policies};
        {aborted, Reason} ->
            io:format("Failed to populate access policies: ~p~n", [Reason]),
            {error, Reason}
    end.
% procura Nodes
init_access_policies(SpreadsheetName, Nodes) ->
    mnesia:transaction(fun() ->
        %% Rimuovi le politiche esistenti per lo spreadsheet
        mnesia:delete({access_policies, SpreadsheetName}),
        %% Inserisci le nuove politiche
        lists:foreach(fun(Node) ->
            Record = #access_policies{name = SpreadsheetName, proc = list_to_atom("nodo" ++ atom_to_list(Node)), access = read},
            io:format("Inserting access policy: ~p~n", [Record]),
            mnesia:write(Record)
        end, Nodes),
        ok
    end).
