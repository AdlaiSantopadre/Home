# Documentazione del Modulo `demo_menu.erl`

## Introduzione

Il modulo `demo_menu.erl` fornisce un'interfaccia a menu per eseguire la configurazione del cluster, distribuire i moduli, avviare l'applicazione e accedere a un menu di test delle API dello spreadsheet distribuito.

Questa documentazione guida  attraverso le funzionalità principali del modulo, mostrando il suo listato in output e illustrando il comportamento delle diverse opzioni disponibili.

Preliminarmente occorre impostare l`ambiente di distribuzione della Applicazione.
Basandoci su un S.O. windows 10 è stato predisposto un comando **setup_nodes.bat** che avvia il cluster  Mnesia e il nodo Monitor_service,in dettaglio esso compila i moduli del progetto, ricrea le cartelle di destinazione di Mnesia per ogni nodo, avvia ogni nodo come parte del cluster Mnesia+APPLICATION OTP (eccetto Monitor_service)

---

## Avvio del Menu Principale

Per avviare il menu principale, eseguire:

```erlang 
demo_menu:start().
```

L'output sarà simile al seguente:

```
===========  Setup ====================
1 -> Setup and Start Mnesia(Run Once)
E -> Exit to Shell
======================================
```

Se è la prima esecuzione del menu, verrà chiesto se si desidera eseguire il **Setup di Mnesia**.

```
Do you want to run Setup & Start Mnesia? (y/n): 
```

Se si sceglie `y`, il setup iniziale dello schema di Mnesia verrà eseguito e il sistema continuerà  con altre opzioni per la distribuzione dei moduli e l'avvio dell'applicazione.

```
Mnesia is running on nodes: ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7']
```

Questa operazione imposta lo schema Mnesia e avvia il database distribuito.

### Distribuzione e Compilazione Moduli
Dopo il setup di Mnesia, il menu continua con:

```
========  Setup ==========================
D -> (Re-Compile and Load Modules
S -> Start Distributed Application OTP
E -> Exit to Shell
============================================
```

- `D` **Compila e distribuisce i moduli ai nodi del cluster**
- `S` **Avvia l'applicazione distribuita e passa al menu API Test**

### 3. Avvio dell'Applicazione e Observer
```
Starting Application ...
```

Questa opzione avvia l'applicazione `my_app` sui nodi e avvia `observer:start()` per il monitoraggio.

Dopo questa fase, viene avviato automaticamente il **menu API Test**.

---

## API Test Menu
```
========= API Test Submenu =========
new -> Create new Spreadsheet test_sheet
set -> Set values in Spreadsheet test_sheet
get -> Get value from Spreadsheet test_sheet
info -> Retrieve Spreadsheet info
to -> Export Spreadsheet To CSV
from -> Import Spreadsheet From CSV
share -> Share Spreadsheet
E -> Exit API Test Menu
====================================
```

### Creazione di un Nuovo Spreadsheet
Selezionare `new` per creare un nuovo spreadsheet:
```
Enter number of rows (N) (default: 3): 3
Enter number of columns (M) (default: 4): 4
Enter number of tabs (K) (default: 2): 2
Creating spreadsheet 'test_sheet' with dimensions (3, 4, 2)...
Spreadsheet 'test_sheet' created successfully.
```

### Inserimento di Dati
Selezionare `set` per inserire diversi tipi di dati:
```
Setting some sample values in 'test_sheet' spreadsheet
Executing batch data insertion...
Batch data insertion completed.
```

L'utente può anche inserire valori manualmente.

### Recupero di Dati
Selezionare `get` per recuperare i dati:
```
Retrieving all sample values set in 'test_sheet'
Do you want to read a specific value? (y/n): 
```

L'utente può scegliere di recuperare un valore specifico inserendo tab, riga e colonna.

### Informazioni sullo Spreadsheet
Selezionare `info` per ottenere dettagli su `test_sheet`:
```
Spreadsheet info: #{name => test_sheet, owner => ..., total_tabs => 2, ...}
```

### Esportazione e Importazione CSV
- `to` esporta lo spreadsheet in `demo_sheet.csv`
- `from` importa i dati da `demo_sheet.csv`

### Condivisione dello Spreadsheet
Selezionare `share` per configurare i permessi di accesso:
```
Available nodes: ['nodeAlice@DESKTOPQ2A2FL7', 'nodeBob@DESKTOPQ2A2FL7', 'nodeCharlie@DESKTOPQ2A2FL7']
Enter node name from the list above (or empty to finish): nodeAlice@DESKTOPQ2A2FL7
Enter access policy (read/write): write
Applying access policies: [{'nodeAlice@DESKTOPQ2A2FL7', write}]
```
---




---
## APPENDICE MESSAGGI DA SHELL

## 1. Avvio Monitor_service


```console
erl -sname Monitor_service -setcookie mycookie -pa C:\Users\Campus.uniurb.it\Erlang -eval "cluster_setup:start_cluster()"
```

Utilità da shell

*global:registered_names().*
*observer:start().*
*code:which(node_monitor).* 



## 2.1 SETUP INIZIALE Mnesia dal nodo Alice@DESKTOPQ2A2FL7

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

**distributed_spreadsheet:new(test_sheet).**
**distributed_spreadsheet:new(test_sheet, 3, 4, 2).**

*global:whereis_name(test_sheet).*  Controlla il gen_server globale
*whereis(spreadsheet_sup). % Controlla il supervisore locale*

* per deregistrare un nome
*global:unregister_name(nodeAlice@DESKTOPQ2A2FL7).*

### 3.2 fallimento dello spreadsheet/fallimento del distributed_sup

* da testare
exit(global:whereis_name(test_sheet), kill).
distributed_spreadsheet:new(test_sheet, 3, 4, 2).

## 3.3 Fallimento del node (Alice, Charlie)

**spawn(fun() -> rpc:call('Alice@DESKTOPQ2A2FL7', erlang, halt, []) end).**

* eseguire dal nodo riavviato

**restart_node:setup_mnesia(MnesiaDir)**
**restart_node:start_application().**

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
* c(restart_node).
* c(demo_menu)

## 4. TEST DELLE API

* NOTA per testare da shell, includere prima il comando di registrazione dei record
 **rr("records.hrl").**

* controllare il processo spreadsheet,se necessario
*process_info(global:whereis_name(test_sheet)).*

## 4.1 Test info(Spreadsheetname)

**distributed_spreadsheet:info(test_sheet).**

## 4.2 Test Share(SpreadsheetName,Access_policies)

%% supponendo di aver inizializzato test_sheet e di aver registrato e inserito i nodi con nomi globali nella tabella access_policies
[{nodeAlice@DESKTOPQ2A2FL7,read},{nodeBob@DESKTOPQ2A2FL7,read},{nodeCharlie@DESKTOPQ2A2FL7,read}] %sono le policies iniziali

distributed_spreadsheet:share(test_sheet, [{<0.102.0>, read},{nodeBob@DESKTOPQ2A2FL7,read}]).
**distributed_spreadsheet:share(test_sheet,[{nodeAlice@DESKTOPQ2A2FL7,read},{nodeBob@DESKTOPQ2A2FL7,read},{nodeCharlie@DESKTOPQ2A2FL7,read}]).**
**distributed_spreadsheet:share(test_sheet,[{nodeAlice@DESKTOPQ2A2FL7, write},{nodeBob@DESKTOPQ2A2FL7,read}]).**
**distributed_spreadsheet:share(test_sheet,[{nodeAlice@DESKTOPQ2A2FL7,write},{nodeBob@DESKTOPQ2A2FL7,write},{nodeCharlie@DESKTOPQ2A2FL7,write}]).**

distributed_spreadsheet:share(test_sheet,[{nodeBob@DESKTOPQ2A2FL7,read},{nodeCharlie@DESKTOPQ2A2FL7,read}]).

## 4.3 Test  get(SpreadsheetName, TabIndex, I, J) e set(SpreadsheetName, TabIndex, I, J, Value)

distributed_spreadsheet:get(test_sheet,2,3,4, 3.14).
distributed_spreadsheet:set(test_sheet,2,3,4, "Hey, Adi").
distributed_spreadsheet:set(test_sheet,2,2,4, atomic).
distributed_spreadsheet:set(test_sheet,2,1,4, ["cani","gatti"]).
distributed_spreadsheet:set(test_sheet,2,1,4,"Erlang! #1@rocks").
distributed_spreadsheet:set(test_sheet,2,1,4,{cane,gatto,topo}).
distributed_spreadsheet:set(test_sheet,2,1,4,#{key => value}).

* distributed_spreadsheet:find_global_name(CallerPid).
* distributed_spreadsheet:check_access(CallerPid).

## 4.4 Test to_csv(SpredsheetName, Filename)

**distributed_spreadsheet:to_csv(test_sheet, spreadsheet).**

## 4.5 Test from_csv(Filename)

**distributed_spreadsheet:from_csv("spreadsheet.csv").**

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

### Chiamata da altro nodo

rpc:call('Alice@DESKTOPQ2A2FL7', distributed_spreadsheet, new, [test]).