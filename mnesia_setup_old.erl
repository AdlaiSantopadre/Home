-module(mnesia_setup_old).
-export([setup_mnesia/2, create_tables/1]).

% Funzione per creare lo schema su tutti i nodi
setup_mnesia(Nodes, Dir) ->
    lists:foreach(fun(Node) -> 
        rpc:call(Node, application, set_env, [mnesia, dir, Dir])
    end, Nodes),
    %% Ferma Mnesia su tutti i nodi
    lists:foreach(fun(Node) ->
        rpc:call(Node, mnesia, stop, [])
    end, Nodes),
    % Imposta la directory di Mnesia nel codice
    
    io:format("Impostata directory di Mnesia: ~p~n", [Dir]),
    mnesia:stop(),
    mnesia:create_schema(Nodes),
    mnesia:start(),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    create_tables(Nodes).

% Funzione per creare le tabelle
create_tables(Nodes) ->
    % Creare la tabella per i dati del foglio di calcolo
    mnesia:create_table(spreadsheet_data, [
        {attributes, [name, tab, row, col, value]},
        {disc_copies, Nodes}
    ]),
    % Creare la tabella per le politiche di accesso
    mnesia:create_table(access_policies, [
        {attributes, [spreadsheet_name, proc, access]},
        {disc_copies, Nodes}
    ]),
    ok.
