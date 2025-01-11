# MESSAGGI DA SHELL

## 1. Setup nodi della cluster Application OTP w Mnesia e monitor_service

* avvia il cluster con
**setup_nodes.bat**

* Aggiungi al cluster un nodo di servizio
**erl -sname monitor_service -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -eval "cluster_setup:start_cluster()"**

* dal nodo Alice

## 2.1 compilazione e distribuzione del codice APPLICATION OTP/SETUP + SETUP_MNESIA

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

* saltare ## SETUP del cluster?
* cluster_setup:start_cluster().
*global:registered_names().*
* %% individua la path del codice .beam caricato
*code:which(monitor_service).*

* spostare il codice dentro setup.cluster
**mnesia_setup:setup_mnesia(Nodes, Dirs).** %Nodes dovrà comprendendere "monitor_service@DESKTOPQ2A2FL7"
*observer:start().*

## 2.2 Avvio della APP da nodo Alice

*Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'].*
**mnesia_setup:start_application(Nodes).**
*observer:start().*
*application:which_applications().*

* (solo sul nodo Alice)
*application:start(my_app).*
*supervisor:which_children(app_sup).* >>ritorna [{spreadsheet_supervisor,<0.210.0>,supervisor,[spreadsheet_supervisor]}]
*supervisor:which_children(spreadsheet_sup).* >>ritorna []
*application:which_applications().*

## 3. Avvio  distributed_spreadsheet dal nodo Alice

**distributed_spreadsheet:new(undicigennaio).**
**distributed_spreadsheet:new(undicigennaio, 3, 4, 2).**
>>f(Args).
>>Args= {undicigennaio, 4, 3, 2,self()}.
>>distributed_spreadsheet:start_link(Args).
>>spreadsheet_supervisor:start_spreadsheet(Args).
>>supervisor:start_child(spreadsheet_sup, [Args]).

*supervisor:which_children(spreadsheet_sup).*  >>aggiunto {undefined,<0.119101.0>,worker,[distributed_spreadsheet]}
*global:whereis_name(undicigennaio).*  Controlla il gen_server globale
*whereis(spreadsheet_sup). % Controlla il supervisore locale*

* per deregistrare un nome
*global:unregister_name(nodoAlice@DESKTOPQ2A2FL7).*

### 3.2 fallimento dello spreadsheet/fallimento del distributed_sup

* da testare
exit(global:whereis_name(undicigennaio), kill).
distributed_spreadsheet:new(undicigennaio, 3, 4, 2).

## 3.3 Fallimento del NODO (Alice, Charlie)

**spawn(fun() -> rpc:call('Alice@DESKTOPQ2A2FL7', erlang, halt, []) end).**

* implementare restart_node

erl -sname Alice -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -config Alice
global:registered_names().
spawn(fun() -> rpc:call('Charlie@DESKTOPQ2A2FL7', erlang, halt, [])end).

erl -sname Bob -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -config Bob
erl -sname Charlie -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -config Charlie
erl -sname monitor_service -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang

## 4. TEST DELLE API

* NOTA per testare da shell, includere prima il comando di registrazione dei record
 **rr("records.hrl").**

* controllare il processo spreadsheet,se necessario
*process_info(global:whereis_name(undicigennaio)).*

## 4.1 Test info(Spreadsheetname)

**distributed_spreadsheet:info(undicigennaio).**

## Test Share(SpreadsheetName,Access_policies)

* policies iniziali
[{nodoAlice@DESKTOPQ2A2FL7,write},{nodoBob@DESKTOPQ2A2FL7,read},{nodoCharlie@DESKTOPQ2A2FL7,read},{nodomonitor_service@DESKTOPQ2A2FL7,read}]

* test funzione ausiliaria
*global:whereis_name(nodoAlice@DESKTOPQ2A2FL7).*

distributed_spreadsheet:update_access_policies(undicigennaio, [{<20727.83177.0>, write},{nodoBob@DESKTOPQ2A2FL7,read}]).
distributed_spreadsheet:update_access_policies(undicigennaio,[{nodoAlice@DESKTOPQ2A2FL7,read},{nodoBob@DESKTOPQ2A2FL7,read},{nodoCharlie@DESKTOPQ2A2FL7,read}], [{nodoAlice@DESKTOPQ2A2FL7, write},{nodoBob@DESKTOPQ2A2FL7,read}]).
distributed_spreadsheet:update_access_policies(undicigennaio,[{nodoAlice@DESKTOPQ2A2FL7,write},{nodoBob@DESKTOPQ2A2FL7,read},{nodoCharlie@DESKTOPQ2A2FL7,read}],[{<20727.83177.0>,read}]). Questo comando mi da sintax error
distributed_spreadsheet:share(undicigennaio,[{nodoBob@DESKTOPQ2A2FL7,read},{nodoCharlie@DESKTOPQ2A2FL7,read}]).

## Test get(SpreadsheetName, TabIndex, I, J) e set(SpreadsheetName, TabIndex, I, J, Value)

from node Bob
distributed_spreadsheet:get(undicigennaio,2,3,4).

distributed_spreadsheet:set(undicigennaio,2,3,4, "Hey, Adi").
distributed_spreadsheet:set(undicigennaio,2,2,4, atomic).

distributed_spreadsheet:find_global_name(CallerPid).
distributed_spreadsheet:check_access(CallerPid).

## Test to_csv(SpredsheetName, Filename)

distributed_spreadsheet:to_csv(undicigennaio, spreadsheet).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Compilare  i moduli e distibuirli

Nodes = [ 'Alice@DESKTOPQ2A2FL7','Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'].

Dirs = ["C:/Users/campus.uniurb.it/Erlang/Alice@DESKTOPQ2A2FL7_data",
        "C:/Users/campus.uniurb.it/Erlang/Bob@DESKTOPQ2A2FL7_data",
        "C:/Users/campus.uniurb.it/Erlang/Charlie@DESKTOPQ2A2FL7_data"].

### arrestare ed eliminare lo schema di Mnesia

% su ogni nodo
mnesia:stop().
mnesia:delete_schema([node()]).
q().

% per tutti i nodi dal nodo1
mnesia:stop().
mnesia:delete_schema(['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7']).
init:stop().

* in alternativa crea tre directory separate
* C:\Users\campus.uniurb.it\Erlang\Alice_data
* C:\Users\campus.uniurb.it\Erlang\Bob_data
* C:\Users\campus.uniurb.it\Erlang\Charlie_data
* e in ogni directory avvia un nodo

### Ricollegare il nodo Alice al cluster Mnesia

*mnesia:start().*
mnesia:change_config(extra_db_nodes, [ 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7']).

* Controlla la comunicazione tra i nodi(avviata  nel modulo  node_monitor)
*net_adm:ping('Alice@DESKTOPQ2A2FL7').*
*net_adm:ping('Bob@DESKTOPQ2A2FL7').*
*net_adm:ping('Charlie@DESKTOPQ2A2FL7').*

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
