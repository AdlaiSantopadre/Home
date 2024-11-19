# MESSAGGI DA SHELL

## AVVIO di un Cluster di  (3) Nodi con Directory di Mnesia separate
%%% usa il file setup_nodes.bat
 
%crea tre directory separate
C:\Users\campus.uniurb.it\Erlang\node1_data
C:\Users\campus.uniurb.it\Erlang\node2_data
C:\Users\campus.uniurb.it\Erlang\node3_data

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

## Controlla la comunicazione tra i nodi

observer:start().

net_adm:ping('node1@DESKTOPQ2A2FL7').
net_adm:ping('node2@DESKTOPQ2A2FL7').
net_adm:ping('node3@DESKTOPQ2A2FL7').

## COMPILARE ed ESEGUIRE mnesia_setup

c(mnesia_setup).
Nodes = ['node1@DESKTOPQ2A2FL7', 'node2@DESKTOPQ2A2FL7', 'node3@DESKTOPQ2A2FL7'].
Dirs = ["C:/Users/campus.uniurb.it/Erlang/node1_data",
        "C:/Users/campus.uniurb.it/Erlang/node2_data",
        "C:/Users/campus.uniurb.it/Erlang/node3_data"].

mnesia_setup:setup_mnesia(Nodes, Dirs).

% per ottenere l`elenco dei nodi connessi esplicitamente a formare  un cluster
mnesia:system_info(running_db_nodes).