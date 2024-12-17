# MESSAGGI DA SHELL

## AVVIO / RIAVVIO di un Cluster di (3) Nodi con Directory di Mnesia separate

### arrestare ed eliminare lo schema di Mnesia

% su ogni nodo
mnesia:stop().
mnesia:delete_schema([node()]).
q().

% per tutti i nodi dal nodo1
mnesia:stop().
mnesia:delete_schema(['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7']).
init:stop().

% da una powershell ulteriore avvia il cluster con **setup_nodes.bat**
e avviare
erl -sname node_test -setcookie mycookie


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%in alternativa crea tre directory separate
C:\Users\campus.uniurb.it\Erlang\Alice_data
C:\Users\campus.uniurb.it\Erlang\Bob_data
C:\Users\campus.uniurb.it\Erlang\Charlie_data
%e in ogni directory avvia un nodo
erl -sname Alice -setcookie mycookie
erl -sname Bob -setcookie mycookie
erl -sname Charlie -setcookie mycookie
erl -sname node_test -setcookie mycookie
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## COMPILARE mnesia_setup

%% **dal nodo1**
c(mnesia_setup).

## ESEGUIRE mnesia_setup

### Controlla la comunicazione tra i nodi

net_adm:ping('Alice@DESKTOPQ2A2FL7').
net_adm:ping('Bob@DESKTOPQ2A2FL7').
net_adm:ping('Charlie@DESKTOPQ2A2FL7').
%% **dal nodo Alice**
Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'].
Dirs = ["C:/Users/campus.uniurb.it/Erlang/Alice_data",
        "C:/Users/campus.uniurb.it/Erlang/Bob_data",
        "C:/Users/campus.uniurb.it/Erlang/Charlie_data"].

mnesia_setup:setup_mnesia(Nodes, Dirs).

### creazione delle tabelle

mnesia_setup:create_tables(Nodes).

## messaggi di diagnosi su Mnesia 

% per ottenere l'elenco dei nodi connessi esplicitamente a formare  un cluster
mnesia:system_info(running_db_nodes).
mnesia:system_info().
mnesia:system_info(tables).
mnesia:table_info(spreadsheet_data, all).
mnesia:table_info(spreadsheet_owners, all)
%cancellare una tabella e ricrearla
mnesia:delete_table(Table).
mensia:create_table(....)

### Avvio di mnesia db

mnesia_setup:mnesia_start(Nodes).
observer:start().
%Consulta le tabelle su observer->Applications->Mnesia->Table viewer

### Utilità per cancellare  i dati di una tabella

c(delete_spreadsheet).
delete_spreadsheet:delete_spreadsheet(SpreadsheetName).
%% OSS:alla fine devi cancellare il nome globale registrato !!

## TEST DELLE API

NOTA per testare da shell, includere prima il comando di registrazione dei record
ES rr("records.hrl").

## distribuzione del codice del gen_server e del codice del supervisor

%%dal nodo test
c(distributed_spreadsheet).
c(spreadsheet_supervisor).
Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'].
Modules = [distributed_spreadsheet,spreadsheet_supervisor].
mnesia_setup:distribute_modules(Nodes, Modules).

%% individua la path del codice .beam caricato
code:which(distributed_spreadsheet).

## TEST del supervisore sul nodo Alice

spreadsheet_supervisor:start_link().
spreadsheet_supervisor:add_spreadsheet(quindicinovembre, 3, 4, 2, self()).
supervisor:which_children(spreadsheet_supervisor).

## Avvio del supervisore su tutti i nodi

%% dal nodo Alice
 [rpc:call(Node, spreadsheet_supervisor, start_link, []) || Node <- Nodes].
%% oppure

rpc:call('Bob@DESKTOPQ2A2FL7', spreadsheet_supervisor, start_link, []).
rpc:call('Charlie@DESKTOPQ2A2FL7', spreadsheet_supervisor, start_link, []).
self().


%% avviare il gen_server con un nome = quindicidicembre globale che mi determina
%% global:registered_names().     %%[{dodicidicembre,owner},dodicidicembre]
distributed_spreadsheet:new(dodicidicembre).
%%controlla il processo se necessario
global:registered_names().
global:whereis_name(dodicidicembre).
process_info(global:whereis_name(dodicidicembre)).
%%arrestare gen_server

%% supponendo di aver inizializzato dodicidicembre e di aver registrato i nodi con nomi globali
%% global:register_name(Bob_read_policy, self()).

distributed_spreadsheet:share(dodicidicembre, [{self(), write}]).

**NOTA:questa funzione, lanciata da Alice non gestisce il caso in cui per debug o errore dodicidicembre non è più un nome registrato**


&& da rivedere
## Aggiornamento del codice di gen_server a caldo - Hot Code upgrade

Pid = global:whereis_name(dodicidicembre).
erlang:is_process_alive(Pid).
sys:change_code(Pid, distributed_spreadsheet, undefined, []).
f(Pid). 