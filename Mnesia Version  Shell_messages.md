# MESSAGGI DA SHELL

## AVVIO di un Cluster di  (3) Nodi con Directory di Mnesia separate

%crea tre directory separate
C:\Users\campus.uniurb.it\Erlang\node1_data
C:\Users\campus.uniurb.it\Erlang\node2_data
C:\Users\campus.uniurb.it\Erlang\node3_data 
% Nota q(). chiude la shell

%in ogni directory avvia un nodo
erl -sname node1 -setcookie mycookie

erl -sname node2 -setcookie mycookie

erl -sname node3 -setcookie mycookie

## Comandi per Eliminare-resettare Mnesia

mnesia:stop().
mnesia:delete_schema([node()]).
q().  
mnesia:delete_schema(['node1', 'node2', 'node3']).
init:stop().

## Creazione dello Schema Mnesia

% nel nodo 1
c(mnesia_setup).

Nodes = ['node1', 'node2', 'node3'].
Dir = "C:/Users/campus.uniurb.it/Erlang/node1_data".
mnesia_setup:setup_mnesia(Nodes).

% nel nodo 1 , 2 e 3
mnesia:start().
%nei vari nodi
net_adm:ping('node1@DESKTOPQ2A2FL7').
net_adm:ping('node2@DESKTOPQ2A2FL7').
net_adm:ping('node3@DESKTOPQ2A2FL7').

% Connetti manualmente i nodi
mnesia:change_config(extra_db_nodes, ['node2', 'node3']).



mnesia:change_table_copy_type(schema, node(), disc_copies).
mnesia:change_table_copy_type(spreadsheet_data, node(), disc_copies).
mnesia:change_table_copy_type(access_policies, node(), disc_copies).


% per ottenere l`elenco dei nodi connessi esplicitamente a fprmare  un cluster
mnesia:system_info(running_db_nodes).
