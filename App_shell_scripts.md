# MESSAGGI DA SHELL

## Setup ambiente

% da una powershell ulteriore avvia il cluster con **setup_nodes.bat**
%% **Aggiornato per utilizzaresys.config**
%aggiungere al cluster
erl -sname node_test -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -config 
sys


## Compilare i moduli

c(app_sup).
c(distributed_spreadsheet).
c(spreadsheet_supervisor).
c(my_app).
c(mnesia_setup).

## COMPILARE mnesia_setup

%% **dal nodo Alice**
c(mnesia_setup).

### Controlla la comunicazione tra i nodi

net_adm:ping('Alice@DESKTOPQ2A2FL7').
net_adm:ping('Bob@DESKTOPQ2A2FL7').
net_adm:ping('Charlie@DESKTOPQ2A2FL7').

## distribuzione del codice APPLICATION OTP

Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'].
Modules = [distributed_spreadsheet,spreadsheet_supervisor,my_app,app_sup].
mnesia_setup:distribute_modules(Nodes, Modules).
%% individua la path del codice .beam caricato
code:which(distributed_spreadsheet).

## ESEGUIRE mnesia_setup

### setup e creazione delle tabelle

%% **dal nodo Alice**
Nodes = [ 'Alice@DESKTOPQ2A2FL7','Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'].

Dirs = ["C:/Users/campus.uniurb.it/Erlang/Alice_data",
        "C:/Users/campus.uniurb.it/Erlang/Bob_data",
        "C:/Users/campus.uniurb.it/Erlang/Charlie_data"].

mnesia_setup:setup_mnesia(Nodes, Dirs).
mnesia_setup:create_tables(Nodes). 
observer:start().%se lo integro in setup_mnesia/2 non crea le tabelle

## Avvio della APP da nodo Alice

**code:add_patha("C:/Users/campus.uniurb.it/Erlang/").** %% controlla se serve per leggere my.app.app
Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'].
mnesia_setup:start_application(Nodes).    %% NOTA: il comando per inizializzare app_sup è già in my.app.erl
observer:start().
%Consulta le tabelle su observer->Applications->Mnesia->Table viewer

application:which_applications().
SpreadsheetName = ventiquattrodicembre.
mnesia_setup:init_cluster_policies(Nodes, SpreadsheetName).

%% solo sul nodo Alice
%% application:start(my_app).
supervisor:which_children(app_sup).
>>ritorna [{spreadsheet_supervisor,<0.210.0>,supervisor,[spreadsheet_supervisor]}]
supervisor:which_children(spreadsheet_sup).
>>ritorna []
application:which_applications().
SpreadsheetName = ventiquattrodicembre.
mnesia_setup:init_cluster_policies(Nodes, SpreadsheetName).

## TEST Avvio funzioni distributed_spreadsheet dal nodo Alice

%%f(Args).
%%Args= {ventiquattrodicembre, 4, 3, 2,self()}.
%% distributed_spreadsheet:start_link(Args).
%%spreadsheet_supervisor:start_spreadsheet(Args).
supervisor:start_child(spreadsheet_sup, [Args]).
distributed_spreadsheet:new(ventiquattrodicembre).
**distributed_spreadsheet:new(ventiquattrodicembre, 3, 4, 2).**
supervisor:which_children(spreadsheet_sup). %% aggiunto {undefined,<0.119101.0>,worker,[distributed_spreadsheet]}
global:whereis_name(ventiquattrodicembre). % Controlla il gen_server globale
whereis(spreadsheet_sup). % Controlla il supervisore locale
%% per deregistrare un nome **global:unregister_name(nodoAlice@DESKTOPQ2A2FL7).**

### Test fallimento dello spreadsheet

**exit(global:whereis_name(ventiquattrodicembre), kill).**

### fallimento del distributed_sup

%%da testare

## Fallimento del NODO Alice, Charlie

rpc:call('Alice@DESKTOPQ2A2FL7', erlang, halt, []).
rpc:call('Charlie@DESKTOPQ2A2FL7', erlang, halt, []).

### TEST DELLE API

NOTA per testare da shell, includere prima il comando di registrazione dei record
ES **rr("records.hrl").**

%% x controllare il processo spreadsheet,se necessario
process_info(global:whereis_name(ventiquattrodicembre)).

## Test Share(SpreadsheetName,Access_policies)

%% supponendo di aver inizializzato ventiquattrodicembre e di aver registrato e inserito i nodi con nomi globali nella tabella access_policies
[{nodoAlice@DESKTOPQ2A2FL7,read},{nodoBob@DESKTOPQ2A2FL7,read},{nodoCharlie@DESKTOPQ2A2FL7,read}] %sono le policies iniziali
%% test funzione ausiliaria 
distributed_spreadsheet:update_access_policies(ventiquattrodicembre, [{<0.422.0>, write},{<0.141.0>,read}]).

## Test get(SpreadsheetName, TabIndex, I, J) e set(SpreadsheetName, TabIndex, I, J, Value)

from node Bob
distributed_spreadsheet:get(ventiquattrodicembre,2,3,4).

distributed_spreadsheet:set(ventiquattrodicembre,2,3,4, "Hey, Adi").
distributed_spreadsheet:set(ventiquattrodicembre,2,2,4, atomic).

distributed_spreadsheet:find_global_name(CallerPid).
distributed_spreadsheet:check_access(CallerPid).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### arrestare ed eliminare lo schema di Mnesia

% su ogni nodo
mnesia:stop().
mnesia:delete_schema([node()]).
q().

% per tutti i nodi dal nodo1
mnesia:stop().
mnesia:delete_schema(['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7']).
init:stop().

%%%
%in alternativa crea tre directory separate
C:\Users\campus.uniurb.it\Erlang\Alice_data 
C:\Users\campus.uniurb.it\Erlang\Bob_data
C:\Users\campus.uniurb.it\Erlang\Charlie_data
%e in ogni directory avvia un nodo
erl -sname Alice -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -config sys

erl -sname Bob -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -config sys
erl -sname Charlie -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -config sys

erl -sname node_test -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -config sys


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## messaggi di diagnosi su Mnesia

% per ottenere l'elenco dei nodi connessi esplicitamente a 
% formare un cluster
mnesia:system_info(running_db_nodes).
mnesia:table_info(spreadsheet_data, where_to_read).
%%  verifica dove è replicata la tabella spreadsheet_data
mnesia:system_info().
mnesia:system_info(tables).
mnesia:table_info(spreadsheet_data, all).
mnesia:table_info(spreadsheet_owners, all)
%cancellare una tabella e ricrearla
mnesia:delete_table(Table).
mensia:create_table(....)

