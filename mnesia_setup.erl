-module(mnesia_setup). % v. 3.1 Windows SO con shortNames  + distribuzione del codice compilato

-export([setup_mnesia/2, create_tables/1, mnesia_start/1, distribute_modules/2]).
-include("spreadsheet_data.hrl").
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
    mnesia:create_schema(Nodes),

    %% Avvia Mnesia su tutti i nodi
    lists:foreach(fun(Node) ->
        rpc:call(Node, mnesia, start, [])
    end, Nodes),

    %% Cambia la copia dello schema su disco  %%verifica se necesssario..
    lists:foreach(fun(Node) ->
        rpc:call(Node, mnesia, change_table_copy_type, [schema, Node, disc_copies])
    end, Nodes).
    
    
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
            {attributes, [spreadsheet_name, proc, access]},
            {disc_copies, Nodes}
        ]),
    %% Tabella per i proprietari degli spreadsheet
        mnesia:create_table(spreadsheet_owners, [
            {attributes, [spreadsheet_name, owner]},
            {disc_copies, Nodes}
        ]),
    %% Aggiungere indici per migliorare le query
    ok.
    
mnesia_start(Nodes) ->
    lists:foreach(fun(Node) ->
            rpc:call(Node, mnesia, start, [])
        end, Nodes),
    mnesia:wait_for_tables([access_policies,spreadsheet_data,spreadsheet_owners], 20000).

    

