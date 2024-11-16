-module(mnesia_setup).
-export([setup_mnesia/1, create_tables/0]).

% Funzione per creare lo schema su tutti i nodi
setup_mnesia(Nodes) ->
    % Creare lo schema distribuito
    mnesia:create_schema(Nodes),
    mnesia:start(),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    create_tables().

% Funzione per creare le tabelle
create_tables() ->
    % Creare la tabella per i dati del foglio di calcolo
    mnesia:create_table(spreadsheet_data, [
        {attributes, [name, tab, row, col, value]},
        {disc_copies, mnesia:system_info(running_db_nodes)}
    ]),
    % Creare la tabella per le politiche di accesso
    mnesia:create_table(access_policies, [
        {attributes, [spreadsheet_name, proc, access]},
        {disc_copies, mnesia:system_info(running_db_nodes)}
    ]),
    ok.
