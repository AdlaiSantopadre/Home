# MESSAGGI DA SHELL

## 1.1 Setup nodi della cluster Application OTP w Mnesia e monitor_service

* avvia il cluster con
**setup_nodes.bat**

* Aggiungi al cluster un nodo di servizio
**erl -sname monitor_service -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -eval "cluster_setup:start_cluster()"**

* dal nodo Alice

## 1.2 distribuzione del codice APPLICATION OTP

### Compilare  i moduli della Dist.APPLICATION del node_monitor e di setup

**cluster_setup:setup().**

* c(app_sup).
* c(distributed_spreadsheet).
* c(spreadsheet_supervisor).
* c(my_app).
* c(node_monitor).
* c(restart_node).
* c(cluster_setup).
* c(mnesia_setup).
* Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'].
* Modules = [distributed_spreadsheet,spreadsheet_supervisor,my_app,app_sup,node_monitor,mnesia_setup,cluster_setup,restart_node].
* mnesia_setup:distribute_modules(Nodes, Modules).
*observer:start().*

* saltare ## SETUP del cluster

* cluster_setup:start_cluster().
*global:registered_names().*
* %% individua la path del codice .beam caricato
*code:which(node_monitor).*

* test  codice da rimuovere
**cluster_setup:test_init_access_policies(ventiquattrodicembre).**

## 2. ESEGUIRE mnesia_setup:distribuzione del codice,setup,creazione delle tabelle e avvio di Mnesia

%% **dal primo nodo Alice**

Nodes = [ 'Alice@DESKTOPQ2A2FL7','Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'].

Dirs = ["C:/Users/campus.uniurb.it/Erlang/Alice@DESKTOPQ2A2FL7_data",
        "C:/Users/campus.uniurb.it/Erlang/Bob@DESKTOPQ2A2FL7_data",
        "C:/Users/campus.uniurb.it/Erlang/Charlie@DESKTOPQ2A2FL7_data"].

mnesia_setup:setup_mnesia(Nodes, Dirs).
%%**Attenzione alla crezione delle tabelle**
mnesia_setup:create_tables(Nodes).
*observer:start().*

## 3.Avvio della APP da nodo Alice

*Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'].*
mnesia_setup:start_application(Nodes).

 %% NOTA: il comando per inizializzare app_sup è già in my.app.erl
*observer:start().*

* Controlla la comunicazione tra i nodi(si avvia nel node_monitor)

*net_adm:ping('Alice@DESKTOPQ2A2FL7').*
*net_adm:ping('Bob@DESKTOPQ2A2FL7').*
*net_adm:ping('Charlie@DESKTOPQ2A2FL7').*

## Avvio di un distributed spreadsheet

*application:which_applications().*

* (solo sul nodo Alice)

%% application:start(my_app).
supervisor:which_children(app_sup).
>>ritorna [{spreadsheet_supervisor,<0.210.0>,supervisor,[spreadsheet_supervisor]}]
supervisor:which_children(spreadsheet_sup).
>>ritorna []
*application:which_applications().*

## 4. Avvio  distributed_spreadsheet dal nodo Alice

distributed_spreadsheet:new(ventiquattrodicembre).
**distributed_spreadsheet:new(ventiquattrodicembre, 3, 4, 2).**
>>f(Args).
>>Args= {ventiquattrodicembre, 4, 3, 2,self()}.
>>distributed_spreadsheet:start_link(Args).
>>spreadsheet_supervisor:start_spreadsheet(Args).
>>supervisor:start_child(spreadsheet_sup, [Args]).

*supervisor:which_children(spreadsheet_sup).*  aggiunto {undefined,<0.119101.0>,worker,[distributed_spreadsheet]}
*global:whereis_name(ventiquattrodicembre).*  Controlla il gen_server globale
*whereis(spreadsheet_sup). % Controlla il supervisore locale*

* per deregistrare un nome
*global:unregister_name(nodoAlice@DESKTOPQ2A2FL7).*

### fallimento dello spreadsheet/fallimento del distributed_sup

%%da testare
distributed_spreadsheet:new(ventiquattrodicembre, 3, 4, 2).
exit(global:whereis_name(ventiquattrodicembre), kill).

## Fallimento del NODO (Alice, Charlie)

spawn(fun() -> rpc:call('Alice@DESKTOPQ2A2FL7', erlang, halt, []) end).
erl -sname Alice -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -config Alice
global:registered_names().
spawn(fun() -> rpc:call('Charlie@DESKTOPQ2A2FL7', erlang, halt, [])end).

erl -sname Bob -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -config Bob
erl -sname Charlie -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -config Charlie

erl -sname monitor_service -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang

### Ricollegare il nodo Alice al cluster Mnesia

mnesia:start().
mnesia:change_config(extra_db_nodes, [ 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7']).

### TEST DELLE API

NOTA per testare da shell, includere prima il comando di registrazione dei record
ES **rr("records.hrl").**

%% x controllare il processo spreadsheet,se necessario
process_info(global:whereis_name(ventiquattrodicembre)).

## Test Share(SpreadsheetName,Access_policies)

%% supponendo di aver inizializzato ventiquattrodicembre e di aver registrato e inserito i nodi con nomi globali nella tabella access_policies
[{nodoAlice@DESKTOPQ2A2FL7,read},{nodoBob@DESKTOPQ2A2FL7,read},{nodoCharlie@DESKTOPQ2A2FL7,read}] %sono le policies iniziali
%% test funzione ausiliaria 
distributed_spreadsheet:update_access_policies(ventiquattrodicembre, [{<20727.83177.0>, write},{nodoBob@DESKTOPQ2A2FL7,read}]).
distributed_spreadsheet:update_access_policies(ventiquattrodicembre,[{nodoAlice@DESKTOPQ2A2FL7,read},{nodoBob@DESKTOPQ2A2FL7,read},{nodoCharlie@DESKTOPQ2A2FL7,read}], [{nodoAlice@DESKTOPQ2A2FL7, write},{nodoBob@DESKTOPQ2A2FL7,read}]).
distributed_spreadsheet:update_access_policies(ventiquattrodicembre,[{nodoAlice@DESKTOPQ2A2FL7,write},{nodoBob@DESKTOPQ2A2FL7,read},{nodoCharlie@DESKTOPQ2A2FL7,read}],[{<20727.83177.0>,read}]). Questo comando mi da sintax error
distributed_spreadsheet:share(ventiquattrodicembre,[{nodoBob@DESKTOPQ2A2FL7,read},{nodoCharlie@DESKTOPQ2A2FL7,read}]).


## Test get(SpreadsheetName, TabIndex, I, J) e set(SpreadsheetName, TabIndex, I, J, Value)

from node Bob
distributed_spreadsheet:get(ventiquattrodicembre,2,3,4).

distributed_spreadsheet:set(ventiquattrodicembre,2,3,4, "Hey, Adi").
distributed_spreadsheet:set(ventiquattrodicembre,2,2,4, atomic).

distributed_spreadsheet:find_global_name(CallerPid).
distributed_spreadsheet:check_access(CallerPid).

## Test info(Spreadsheetname)

distributed_spreadsheet:info(ventiquattrodicembre).

## Test to_csv(SpredsheetName, Filename)

distributed_spreadsheet:to_csv(ventiquattrodicembre, spreadsheet).

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
mnesia:force_load_table(spreadsheet_data). %% forza caricamento tabella 
%cancellare una tabella e ricrearla
mnesia:delete_table(Table).
mensia:create_table(....)

