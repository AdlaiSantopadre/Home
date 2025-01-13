# ADCC 2023-2024

* Per ogni progetto bisogna consegnare una relazione scritta (pdf) e un link ad un repository git con il codice
* Nella relazione documentate tutte le vostre scelte implementative (design choices)
* La consegna avviene una settimana prima della data dell’esame

## Distributed Spreadsheet

• Un foglio di calcolo è una “matrice” di NxM celle
• Una cella può contenere
• qualsiasi tipo di dato primitivo
• Il valore undef

Intefaccia 1/4
Il modulo spreadsheet deve contenere almeno le seguenti funzioni:
new(name) -> spreadsheet | {error,reason}
• Crea un nuovo foglio di nome “name” di dimensioni NxM di K tab (schede)
• assegna il processo creatore come proprietario del foglio
• I parametri N, M, K  sono default nel modulo

new(name, N, M, K) -> spreadsheet | {error,reason} • Crea un nuovo foglio di nome “name” di K tab
• Ogni tab ha dimensioni NxM
• assegna il processo creatore come proprietario del foglio

share(spreadsheet, AccessPolicies) -> **bool**
Il proprietario del foglio può condidivere il foglio in Lettura o scrittura con altri processi
 AccessPolicies è una lista di {Proc,AP} dove
 • Proc è un Pid/reg_name • AP = read | write
 Le policy di accesso ad un foglio possono cambiare in qualsiasi momento

get(spreadsheet, tab, i, j, val) -> Value | undef
• Legge il valore della cella (i,j) che appartiene al tab del foglio name
set(spreadsheet, tab, i, j, k, val) -> bool
• Scrive il valore della cella (i,j) che appartiene al tab del foglio name
get(spreadsheet, tab, i, j, val, timeout) -> Value | undef | timeout
set(spreadsheet, tab, i, j, val, timeout) -> bool | timeout

to_cvs(spreadsheet,filename) -> ok | {error,reason}
• Esporta in cvs (comma separated values) il foglio

from_cvs(filename) -> spreadsheet | {error,reason}

to_cvs(name,filename, timeout) -> ok | {error,reason} | timeout

from_cvs(name,filename, timeout)->spreadsheet | {error,reason} | timeout

info(name) -> Spreadsheet_info Le informazioni devono contenere almeno:
• Numerodi celle x tab
• I permessi di lettura e scrittura

Requisiti
• I fogli devono essere visibili su tutti i nodi della rete
• I fogli devono resistere ai fallimenti di uno o più nodi • Esempio: la mia applicazione esegue su tre nodi, 2 cadono ma i fogli sono ancora visibili al nodo rimanente

## Spreadsheet avanzato

 Prerequisiti: • Aver implementato la parte “semplice”
 Aggiungiamo operazioni che modificano la forma del foglio
 add_row(spreadsheet, tab) -> ok | {err,reason}
 • Aggiunge una riga in append al tab del foglio name
 del_row(spreadsheet, tab,i) -> ok | {err,reason}
 • Rimuove l’iesima riga del tab del foglio nameSpreadsheet avanzato 2/2
 Una cella può contenere
 • qualsiasi tipo di dato primitivo
 • Il valore undef
  
 • una formula/macro
Suggerimenti
LE FORMULE SONO FUNZIONI LAMBDA
UNA CELLA PUÒ ESSERE VISTA COME UNA MEMORY CELL DI CCS/PI-CALCOLO
Formule
• I valori delle celle possono essere delle formule
• Le formule possono usare dei range di celle
• Per semplicità usiamo range regolari del tipo a1:c6 • a1:c6 indica tutte le seguenti celle
• tab!a1:c6 indica tutte le celle comprese nel range a1:c6 del tab1
