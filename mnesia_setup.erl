-module(mnesia_setup). % v. 4.2 Windows SO con shortNames  + distribuzione del codice compilato 
%% Record spreadsheet_info
-export([setup_mnesia/2,create_tables/1, distribute_modules/2,start_application/1, init_cluster_policies/2]).%,  mnesia_start/1,

-include("records.hrl").


setup_mnesia(Nodes, Dirs) ->
    %% Imposta la directory di Mnesia per ogni nodo
    lists:zipwith(fun(Node, Dir) -> 
        rpc:call(Node, application, set_env, [mnesia, dir, Dir])
    end, Nodes, Dirs),

    %% Ferma Mnesia su tutti i nodi
    lists:foreach(fun(Node) ->
        rpc:call(Node, mnesia, stop, [])
    end, Nodes),

    %% Cancella schema precedente su tutti i nodi
    lists:foreach(fun(Node) ->
        rpc:call(Node, mnesia, delete_schema, [Nodes])
    end, Nodes),

    %% Crea lo schema sui nodi specificati
    mnesia:create_schema(Nodes).
%%verifica se necessario..
    % %% Cambia la copia dello schema su disco  
    % lists:foreach(fun(Node) ->
    %     rpc:call(Node, mnesia, change_table_copy_type, [schema, Node, disc_copies])
    % end, Nodes).
    
    

create_tables(Nodes) ->
    %% Creare la tabella per i dati del foglio di calcolo con replica
    mnesia:create_table(spreadsheet_data, [
        {attributes, record_info(fields, spreadsheet_data)},
        {type, bag}, 
        {disc_copies, Nodes},
        {index, [tab, row, col]} % Indici per ottimizzare le query
        ]),
    %% Creare la tabella per le politiche di accesso con replica
    mnesia:create_table(access_policies, [
        {attributes, record_info(fields,access_policies)},
        {type, bag}, 
        {disc_copies, Nodes}
    ]),
    % %% Tabella per i proprietari degli spreadsheet
    %     mnesia:create_table(spreadsheet_owners, [
    %     {attributes, record_info(fields,spreadsheet_owners)},
    %     {disc_copies, Nodes}
    % ]),
    %% Tabella metadati degli spreadsheet
        mnesia:create_table(spreadsheet_info, [
        {attributes, record_info(fields, spreadsheet_info)},
        {disc_copies, Nodes}]),

        
    lists:foreach(fun(Node) ->
            rpc:call(Node, mnesia, start, [])
        end, Nodes),
    mnesia:wait_for_tables([access_policies,spreadsheet_data,spreadsheet_owners,spreadsheet_info], 20000),
    
    %% Avvia Mnesia su tutti i nodi
    lists:foreach(fun(Node) ->
        rpc:call(Node, mnesia, start, [])
    end, Nodes).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Avvia my_app su tutti i nodi
start_application(Nodes) ->
    lists:foreach(fun(Node) ->
        rpc:call(Node, application, start, [my_app])
    end, Nodes).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Distribuisci i moduli specificati su tutti i nodi
distribute_modules(Nodes, Modules) ->
    lists:foreach(fun(Module) ->
        lists:foreach(fun(Node) ->
            case file:read_file(atom_to_list(Module) ++ ".beam") of
                {ok, Binary} ->
                    case rpc:call(Node, code, load_binary, [Module, atom_to_list(Module) ++ ".beam", Binary]) of
                        {module, Module} ->
                            io:format("Module ~p successfully loaded on node ~p~n", [Module, Node]);
                            
                        {error, Reason} ->
                            io:format("Failed to load module ~p on node ~p: ~p~n", [Module, Node, Reason])
                    end;
                {error, Reason} ->
                    io:format("Failed to read module ~p: ~p~n", [Module, Reason])
            end
        end, Nodes)
    end, Modules).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% inizializza la tabella con le access policies per ogni nodo del cluster
init_cluster_policies(Nodes, SpreadsheetName) ->
    %% Recupera il MasterPid per ogni nodo e registra globalmente il nome
    Policies = lists:map(fun(Node) ->
        %% Esegue il comando su ciascun nodo
        case rpc:call(Node, application_controller, get_master, [my_app]) of
            MasterPid when is_pid(MasterPid) ->
            
            %% Crea un atomo unico per il nodo
            GlobalName = list_to_atom("nodo" ++ atom_to_list(Node)),
            %% Registra globalmente il MasterPid
            case global:register_name(GlobalName, MasterPid) of
                yes ->
                    io:format("MasterPid ~p registered globally as ~p~n", [MasterPid, GlobalName]),
                    {GlobalName, read}; %tutti i nodo del cluster vengono inseriti con policy read
                no ->
                    io:format("Failed to register MasterPid ~p: ~n", [MasterPid]),
                    {GlobalName,error}
            end;
            undefined ->
                io:format("Failed to retrieve MasterPid from node ~p~n", [Node]),
                {error, undefined}
        end
    end, Nodes),
    io:format("Access policies  for spreadsheet ~p: ~p~n", [SpreadsheetName, Policies]),
    %% Inserisce le politiche di accesso nella tabella Mnesia
    case populate_access_policies(SpreadsheetName, Policies) of
        {atomic,ok} ->
            io:format("Access policies initialized for spreadsheet ~p: ~p~n", [SpreadsheetName, Policies]),
            {ok, Policies};
        {aborted, Reason} ->
            io:format("Failed to populate access policies: ~p~n", [Reason]),
            {error, Reason}
    end.
populate_access_policies(SpreadsheetName, Policies) ->
    mnesia:transaction(fun() ->
        %% Rimuovi le politiche esistenti per lo spreadsheet
        mnesia:delete({access_policies, SpreadsheetName}),
        %% Inserisci le nuove politiche
        lists:foreach(fun({ProcOrName, Access}) ->
            Record = #access_policies{name = SpreadsheetName, proc = ProcOrName, access = Access},
            io:format("Inserting access policy: ~p~n", [Record]),
            mnesia:write(Record)
        end, Policies),
        ok
    end).
        

