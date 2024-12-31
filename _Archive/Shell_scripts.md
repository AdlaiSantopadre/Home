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

%% **dal nodo Alice**
c(mnesia_setup).

## ESEGUIRE mnesia_setup

### Controlla la comunicazione tra i nodi

net_adm:ping('Alice@DESKTOPQ2A2FL7').
net_adm:ping('Bob@DESKTOPQ2A2FL7').
net_adm:ping('Charlie@DESKTOPQ2A2FL7').

### setup e creazione delle tabelle

%% **dal nodo Alice**
Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'].
Dirs = ["C:/Users/campus.uniurb.it/Erlang/Alice_data",
        "C:/Users/campus.uniurb.it/Erlang/Bob_data",
        "C:/Users/campus.uniurb.it/Erlang/Charlie_data"].

mnesia_setup:setup_mnesia(Nodes, Dirs).

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

%Consulta le tabelle su observer->Applications->Mnesia->Table viewer

### Utilità per cancellare  i dati di una tabella

c(delete_spreadsheet).
delete_spreadsheet:delete_spreadsheet(SpreadsheetName).
%% OSS:alla fine devi cancellare il nome globale registrato !!



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

distributed_spreadsheet:new(diciottodicembre, 3, 4, 2).
whereis(supervisor). % Controlla il supervisore locale
global:whereis_name(diciottodicembre). % Controlla il gen_server globale
supervisor:which_children(spreadsheet_supervisor).

### fallimento del supervisor

exit(whereis(supervisor), kill).
global:whereis_name(diciottodicembre). % Deve restituire `undefined`

### fallimento dello spreadsheet

distributed_spreadsheet:new(diciottodicembre, 3, 4, 2).
exit(global:whereis_name(diciottodicembre), kill).

## Avvio del supervisore su tutti i nodi

%% dal nodo Alice
 [rpc:call(Node, spreadsheet_supervisor, start_link, []) || Node <- Nodes].
%% oppure
rpc:call('Alice@DESKTOPQ2A2FL7', spreadsheet_supervisor, start_link, []).
rpc:call('Bob@DESKTOPQ2A2FL7', spreadsheet_supervisor, start_link, []).
rpc:call('Charlie@DESKTOPQ2A2FL7', spreadsheet_supervisor, start_link, []).
self().

## TEST DELLE API

NOTA per testare da shell, includere prima il comando di registrazione dei record
ES rr("records.hrl").

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

