# MESSAGGI DA SHELL

## AVVIO / RIAVVIO di un Cluster di (3) Nodi con Directory di Mnesia separate

### arrestare ed eliminare lo schema di Mnesia

% su ogni nodo
mnesia:stop().
mnesia:delete_schema([node()]).
q().

% su tutti i nodi dal nodo1
mnesia:stop().
mnesia:delete_schema(['node1@DESKTOPQ2A2FL7', 'node2@DESKTOPQ2A2FL7', 'node3@DESKTOPQ2A2FL7']).
init:stop().

% da una powershell ulteriore avvia il cluster con **setup_nodes.bat**

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%in alternativa crea tre directory separate
C:\Users\campus.uniurb.it\Erlang\node1_data
C:\Users\campus.uniurb.it\Erlang\node2_data
C:\Users\campus.uniurb.it\Erlang\node3_data
%e in ogni directory avvia un nodo
erl -sname node1 -setcookie mycookie
erl -sname node2 -setcookie mycookie
erl -sname node3 -setcookie mycookie
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## COMPILARE mnesia_setup

%% **dal nodo1**
c(mnesia_setup).

## ESEGUIRE mnesia_setup

### Controlla la comunicazione tra i nodi

net_adm:ping('node1@DESKTOPQ2A2FL7').
net_adm:ping('node2@DESKTOPQ2A2FL7').
net_adm:ping('node3@DESKTOPQ2A2FL7').
%% **dal nodo1**
Nodes = ['node1@DESKTOPQ2A2FL7', 'node2@DESKTOPQ2A2FL7', 'node3@DESKTOPQ2A2FL7'].
Dirs = ["C:/Users/campus.uniurb.it/Erlang/node1_data",
        "C:/Users/campus.uniurb.it/Erlang/node2_data",
        "C:/Users/campus.uniurb.it/Erlang/node3_data"].

mnesia_setup:setup_mnesia(Nodes, Dirs).

### creazione delle tabelle

mnesia_setup:create_tables(Nodes).

% per ottenere l'elenco dei nodi connessi esplicitamente a formare  un cluster
mnesia:system_info(running_db_nodes).
mnesia:system_info().

### Avvio di mnesia db

mnesia_setup:mnesia_start(Nodes).
observer:start().

## Creare uno spreadsheet distribuito

c(distributed_spreadsheet).
code:which(distributed_spreadsheet).  %% individua la path del codice .beam caricato

distributed_spreadsheet:new(my_spreadsheet).
%Consulta le tabelle su observer->Applications->Mnesia->Table viewer

## Utilità per cancellare  i dati di una tabella

c(delete_spreadsheet).
delete_spreadsheet:delete_spreadsheet(SpreadsheetName).
%% OSS:alla fine  cancella il nome globale registrato !!


## TEST DELLE API

## Test del gen_server

c(distribute_spreadsheet).
Nodes = ['node1@DESKTOPQ2A2FL7', 'node2@DESKTOPQ2A2FL7', 'node3@DESKTOPQ2A2FL7'].
Modules = [distributed_spreadsheet].
mnesia_setup:distribute_modules(Nodes, Modules).




%%avviare il gateway
spreadsheet_gateway:start_link().
%%controlla il processo se necessario
global:registered_names().
global:whereis_name(spreadsheet_gateway).
process_info(global:whereis_name(spreadsheet_gateway)).
%% supponendo di aver inizializzato spreadsheet_2
AccessPolicies = [{self(), write}, {some_other_proc, read}].

spreadsheet_gateway:modify_access(spreadsheet_2, [{self(), write}]).
