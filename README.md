# MESSAGGI

## 1. Avvio nodi della cluster Application OTP w Mnesia e monitor_service DA SHELL WINDOWS

* avvia il cluster di Mnesia + il nodo monitor service
**setup_nodes.bat**

*erl -sname monitor_service -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -eval "cluster_setup:start_cluster()"*

*observer:start().*
*global:registered_names().*
*code:which(node_monitor).* %individua la path del codice .beam caricato

## 2.1 compilazione e distribuzione del codice APPLICATION OTP/SETUP

*c(cluster_setup).*
**cluster_setup:setup().**

## 2.1 SETUP INIZIALE MNESIA dal nodo Alice@DESKTOPQ2A2FL7

*Nodes = [ 'Alice@DESKTOPQ2A2FL7','Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'].
Dirs = ["C:/Users/campus.uniurb.it/Erlang/Alice@DESKTOPQ2A2FL7_data","C:/Users/campus.uniurb.it/Erlang/Bob@DESKTOPQ2A2FL7_data","C:/Users/campus.uniurb.it/Erlang/Charlie@DESKTOPQ2A2FL7_data"].
cluster_setup:setup_mnesia(Nodes,Dirs).*
*mnesia:system_info(running_db_nodes).*

## 2.2 Avvio della APP dal nodo Alice@DESKTOPQ2A2FL7

* (solo sul node Alice)
*Nodes = ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'].*
**cluster_setup:start_application(Nodes).**
*observer:start().*
*application:which_applications().*

*application:start(my_app).*
*supervisor:which_children(app_sup).* >>ritorna [{spreadsheet_supervisor,<0.210.0>,supervisor,[spreadsheet_supervisor]}]
*supervisor:which_children(spreadsheet_sup).* >>ritorna []
*application:which_applications().*

## 3. Avvio  distributed_spreadsheet dal node Alice

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

*global:unregister_name(nodeAlice@DESKTOPQ2A2FL7).*

### 3.2 fallimento dello spreadsheet/fallimento del distributed_sup

* da testare
exit(global:whereis_name(undicigennaio), kill).
distributed_spreadsheet:new(undicigennaio, 3, 4, 2).

## 3.3 Fallimento del node (Alice, Charlie)

**spawn(fun() -> rpc:call('Alice@DESKTOPQ2A2FL7', erlang, halt, []) end).**

* eseguire dal nodo riavviato
**restart_node:setup_mnesia()**

erl -sname Alice -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -config Alice
global:registered_names().
spawn(fun() -> rpc:call('Charlie@DESKTOPQ2A2FL7', erlang, halt, [])end).

erl -sname Bob -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -config Bob
erl -sname Charlie -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -config Charlie
erl -sname monitor_service -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang

### 3.3.1 EVENTUALE(Debugging)Ricompilare  i moduli e distibuirli

*Nodes = [ 'Alice@DESKTOPQ2A2FL7','Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'].*
*Modules = [distributed_spreadsheet,spreadsheet_supervisor,my_app,app_sup,node_monitor,cluster_setup,restart_node].*

**cluster_setup:distribute_modules(Nodes, Modules)**

* c(app_sup).
* c(distributed_spreadsheet).
* c(spreadsheet_supervisor).
* c(my_app).
* c(node_monitor).
* c(restart_node).
* c(cluster_setup).

## 4. TEST DELLE API

* NOTA per testare da shell, includere prima il comando di registrazione dei record
 **rr("records.hrl").**

* controllare il processo spreadsheet,se necessario
*process_info(global:whereis_name(undicigennaio)).*

## 4.1 Test info(Spreadsheetname)

**distributed_spreadsheet:info(undicigennaio).**

## 4.2 Test Share(SpreadsheetName,Access_policies)

%% supponendo di aver inizializzato undicigennaio e di aver registrato e inserito i nodi con nomi globali nella tabella access_policies
[{nodeAlice@DESKTOPQ2A2FL7,read},{nodeBob@DESKTOPQ2A2FL7,read},{nodeCharlie@DESKTOPQ2A2FL7,read}] %sono le policies iniziali

distributed_spreadsheet:share(undicigennaio, [{<0.102.0>, read},{nodeBob@DESKTOPQ2A2FL7,read}]).
**distributed_spreadsheet:share(undicigennaio,[{nodeAlice@DESKTOPQ2A2FL7,read},{nodeBob@DESKTOPQ2A2FL7,read},{nodeCharlie@DESKTOPQ2A2FL7,read}]).**
**distributed_spreadsheet:share(undicigennaio,[{nodeAlice@DESKTOPQ2A2FL7, write},{nodeBob@DESKTOPQ2A2FL7,read}]).**
**distributed_spreadsheet:share(undicigennaio,[{nodeAlice@DESKTOPQ2A2FL7,write},{nodeBob@DESKTOPQ2A2FL7,write},{nodeCharlie@DESKTOPQ2A2FL7,write}]).**

distributed_spreadsheet:share(undicigennaio,[{nodeBob@DESKTOPQ2A2FL7,read},{nodeCharlie@DESKTOPQ2A2FL7,read}]).

## 4.3 Test  get(SpreadsheetName, TabIndex, I, J) e set(SpreadsheetName, TabIndex, I, J, Value)

**distributed_spreadsheet:get(undicigennaio,2,3,4).**

**distributed_spreadsheet:set(undicigennaio,2,3,4, "Hey, Adi").**
**distributed_spreadsheet:set(undicigennaio,2,2,4, atomic).**

* distributed_spreadsheet:find_global_name(CallerPid).
* distributed_spreadsheet:check_access(CallerPid).

## 4.4 Test to_csv(SpredsheetName, Filename)

distributed_spreadsheet:to_csv(undicigennaio, spreadsheet).

## 4.5 Test from_csv(Filename)

distributed_spreadsheet:from_csv("spreadsheet.csv").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

### Compilare  i moduli e distibuirli

Nodes = [ 'Alice@DESKTOPQ2A2FL7','Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7'].

Dirs = ["C:/Users/campus.uniurb.it/Erlang/Alice@DESKTOPQ2A2FL7_data",
        "C:/Users/campus.uniurb.it/Erlang/Bob@DESKTOPQ2A2FL7_data",
        "C:/Users/campus.uniurb.it/Erlang/Charlie@DESKTOPQ2A2FL7_data"].
cluster_setup:distribute_modules(Nodes, Modules)

### arrestare ed eliminare lo schema di Mnesia

% su ogni node
mnesia:stop().
mnesia:delete_schema([node()]).
q().

% per tutti i nodi dal node1
mnesia:stop().
mnesia:delete_schema(['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7']).
init:stop().

* in alternativa crea tre directory separate
* C:\Users\campus.uniurb.it\Erlang\Alice_data
* C:\Users\campus.uniurb.it\Erlang\Bob_data
* C:\Users\campus.uniurb.it\Erlang\Charlie_data
* e in ogni directory avvia un node

### Ricollegare il node Alice al cluster Mnesia

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
