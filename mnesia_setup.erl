-module(mnesia_setup). %Windows SO con shortNames
-export([setup_mnesia/2, create_tables/1]).

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

    %% Cambia la copia dello schema su disco
    lists:foreach(fun(Node) ->
        rpc:call(Node, mnesia, change_table_copy_type, [schema, Node, disc_copies])
    end, Nodes),

    create_tables(Nodes).

create_tables(Nodes) ->
    %% Creare la tabella per i dati del foglio di calcolo con replica
    mnesia:create_table(spreadsheet_data, [
        {attributes, [name, tab, row, col, value]},
        {disc_copies, Nodes}
    ]),
    %% Creare la tabella per le politiche di accesso con replica
    mnesia:create_table(access_policies, [
        {attributes, [spreadsheet_name, proc, access]},
        {disc_copies, Nodes}
    ]),
    ok.

