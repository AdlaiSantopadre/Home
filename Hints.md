# Hints

## Tipi di dati primitivi

Numeri
Interi: Non c'è un limite fisso alla dimensione degli interi, sono solo limitati dalla memoria disponibile.
Numeri in virgola mobile: Rappresentati con una precisione doppia secondo lo standard IEEE.

Atomi
costanti il cui nome è il loro valore
Booleani
In Erlang, i booleani sono rappresentati dagli atomi true e false. Non sono un tipo distinto ma sono atomi usati convenzionalmente come valori booleani.

Tuple
Le tuple sono raccolte fisse di valori (potenzialmente di tipi diversi) raggruppate insieme. Le tuple sono indicate da parentesi graffe. Es {1, 2}, {ok, 123}, {error, "not_found"}

Liste
Le liste in Erlang sono sequenze di elementi. Sono fondamentalmente liste concatenate. Es [1, 2, 3, 4], ["hello", "world"], [a, {2, b}, [1, 2]]

Stringhe
Le stringhe in Erlang sono rappresentate come liste di interi, dove ogni intero rappresenta un codice ASCII del carattere corrispondente.

Binari
I binari sono sequenze di byte utilizzate per gestire dati come immagini, files, o qualsiasi flusso di dati non testuale. Sono indicati da << >>. Es <<1, 2, 3, 4>>, <<255>>, <<"hello">>

Funzioni
In Erlang, anche le funzioni sono considerate come un tipo di dato primitivo, dato che è un linguaggio funzionale. Le funzioni possono essere anonime o possono avere un nome.Fun = fun(X) -> X * 2 end

## net_adm:ping/1

Quando esegui net_adm:ping(node@localhost), stai inviando una richiesta di ping al nodo Erlang specificato.Se il nodo è raggiungibile e la comunicazione è stabilita correttamente, il nodo destinatario risponde:

- pong viene restituito se il nodo è raggiungibile e risponde correttamente. Questo indica che la connettività di rete tra i nodi è attiva e funzionante.
- pang viene restituito se il nodo non è raggiungibile o non risponde. Questo può accadere per vari motivi, come problemi di rete, configurazioni errate, o perché il nodo destinatario non è attivo.

## passaggio parametri di un record

quando passi un record come parametro, se non hai bisogno di aggiornare tutti i campi, puoi aggiornare solo quelli specifici e lasciare il resto invariato con la sintassi Record#record_name{campo = valore}.
 Tuttavia, quando stai passando lo stato nel ciclo (loop) , sarebbe preferibile 'includere tutti i campi del record se essi fanno parte dello stato che il processo deve gestire'

## lists:filter/2

lists:filter/2 è una funzione della libreria standard di Erlang che prende una funzione (in questo caso un funtore) e una lista come argomenti.
Restituisce una nuova lista composta solo dagli elementi che soddisfano la condizione specificata nella funzione.
Esempio
Policies = [
    {proc_a, read},
    {proc_b, write},
    {proc_c, read}
].
Proc = proc_b.
NewPolicies = lists:filter(fun({P,_}) -> P =/= Proc end, Policies)
NewPolicies = [
    {proc_a, read},
    {proc_c, read}
].

## lists:map(Function, List)

applica la funzione specificata a ciascun elemento della lista fornita e restituisce una nuova lista con i risultati;
la lista su cui viene applicata la mappatura è quella generata da **lists:seq(1, N)**

## lists:duplicate(M, undef)

crea una nuova lista lunga M di elementi il cui valore è undef per cui tale funzione chiamata su lists:seq(1,N) produce attraverso lists:map una matrice come lista di liste.

lists:map(fun(_) -> lists:duplicate(M, undef) end, lists:seq(1, N))

Here, lists:map applies the function fun(_) -> lists:duplicate(M, undef) end N times, creating a new independent list of M undef elements each time.
Advantage: Each row is a distinct list in memory, *so changes to one row won’t affect others.*

## Meccanismo di passaggio dei messaggi

Ogni processo in Erlang ha un identificatore unico chiamato PID (Process Identifier). Per inviare un messaggio a un processo, è necessario conoscere il suo PID o avere un nome registrato a cui il PID è associato.
Un processo invia un messaggio a un altro processo usando l'operatore **!** (noto come "send operator"). La sintassi è:
PidOrName ! Message
Quando un messaggio è inviato a un processo, viene inserito nella coda di messaggi di quel processo. Erlang gestisce queste code internamente, e ogni processo ha la propria coda di messaggi isolata.
Il processo destinatario riceve messaggi estratti dalla sua coda di messaggi usando il costrutto **receive**. Il processo può specificare pattern per filtrare i messaggi che desidera trattare, come mostrato di seguito:
receive
    Pattern1 ->
        % Azioni per Pattern1
    Pattern2 when Guard ->
        % Azioni per Pattern2 con guardia
    ...
    after Timeout ->
        % Azioni dopo scaduto il timeout (in millisecondi)
end
*Non-blocking vs Blocking*: La ricezione è di norma un'operazione bloccante, il che significa che il processo resta in attesa di messaggi se la coda è vuota. Tuttavia, è possibile specificare un timeout dopo il quale il processo può eseguire altre azioni se nessun messaggio appropriato è stato ricevuto.

Il ciclo di vita di un processo in Erlang puo essere gestito d una funzione progettata per ricevere e gestire messaggi in modo continuo, mantenendo lo stato aggiornato del processo. Se il processo termina lo stato non sarà più mantenuto e non sarà possibile inviare ulteriori richieste al processo.
Esempio di funzione (loop/0)

```console
loop() ->
    receive
        {msg, From, Message} ->
            io:format("Received ~p from ~p~n", [Message, From]),
            loop();
        {command, stop} ->
            io:format("Stopping process~n")
    after 5000 ->  % Timeout di 5000 millisecondi (5 secondi)
        io:format("No messages received in 5 seconds, looping~n"),
        loop()
    end.
```

## Liste in erlang

Le liste sono immutabili in erlang, ovvero non si possono modificare direttamente.Pertanto per cambiare un elementoin una lista si divide la lista in due parti (precedente e antecedente l`elemento) e si ricostruisce una lista con il nuovo elemento nella posizione voluta.

## Tracciare i Messaggi con dbg

Per tracciare il messaggio contenente State mentre viene inviato, si puo usare il modulo dbg di Erlang. Il modulo dbg permette di tracciare chiamate di funzione e messaggi scambiati tra processi.

Abilitare il Tracing per i Messaggi

```console
dbg:tracer().

dbg:p(all, m).  % Traccia i messaggi tra tutti i processi

```

Questo comando monitorerà tutti i messaggi scambiati tra i processi e mostrerà il contenuto di questi messaggi, incluso il valore di State che viene inviato con il messaggio {spreadsheet_state, State}.
Disattivare il tracing con:

```console
dbg:stop().
```

Ecco un esempio per tracciare l'esecuzione della funzione to_csv/2 e i messaggi tra i processi:

```console
dbg:tracer().
dbg:tpl(spreadsheet, to_csv, x).  % Traccia tutte le chiamate a to_csv/2
dbg:p(all, c).  % Traccia tutte le chiamate di funzione
```

Tracciare i Messaggi Tra Processi

```console
dbg:tracer().
dbg:tpl(spreadsheet, loop, x).  % Traccia la funzione loop/1
dbg:p(all, m).  % Traccia i messaggi tra tutti i processi
```

## Observer

Avvia Observer con il seguente comando nella shell Erlang:

```console
observer:start().
```

Con OObserver puoi seguire
*Processi*: Puoi vedere tutti i processi in esecuzione, il loro PID, e lo stato (sospeso, attivo, ecc.).
Messaggi: Puoi vedere i messaggi inviati e ricevuti da ogni processo.
Supervisori: Se hai un supervisore per il foglio di calcolo, puoi visualizzare l'albero dei processi supervisionati.

## PID

I PID sono l'unico modo per contattare un attore

• Erlang consente di registrare un PID con un atomo particolare e quindi di fare riferimento
ad esso con l'atomo invece che con il PID
È solo un trucco dell'OTP di Erlang, è come una lookup/DNS nelle reti normali

### registered()

restituisce la lista di nomi che sono stati registrati attraverso register/2

### register(Name, Pid)

associa l`atomo Name al processo Pid
Un errore comune con i nomi registrati è che il PID/attore associato ad esso si blocca (ad esempio, non esiste più).
L'invio di un messaggio a un nome registrato che si è bloccato genera un errore dovuto alla funzione di ricerca, che non restituisce un PID

(Ad esempio, in fase di esecuzione il nome registrato viene risolto in un PID, ma se il processo è morto, la funzione di ricerca restituirà *undefined* )
The global:register_name/2 function in Erlang is part of the global module and is used to globally register a process (identified by its PID) with a name that can be accessed across multiple nodes in a distributed system.

Here is the documentation for global:register_name/2:

global:register_name(Name, Pid) -> yes | {error, Reason}
Arguments:
Name: This is an atom or tuple that you want to use as the global identifier for the process.
Pid: The PID of the process that you want to register globally under the given name.
Return Values:
yes: The function returns yes if the registration was successful.
{error, Reason}: If the registration fails, it returns {error, Reason} where Reason is typically an error message indicating why the registration failed. Possible reasons include:
already_registered: This indicates that a process with the same name is already registered globally.
badarg: This indicates that the arguments passed to register_name/2 were invalid (e.g., the name or PID wasn't valid).
mportant Notes:
The Name can be either an atom (like my_process) or a tuple (like {SpreadsheetName, owner} in your case). This allows you to give more structured names.
The Pid must be a valid process identifier (PID) that can be globally referenced.

### Nomi registrati

• Di solito un modello normale per uno spawn/register remoto è di attendere un messaggio dal processo registrato
• Il processo spawned prima si registra con un nome dato e poi conferma allo spawner che è ok

```console
better_reg() ->
    register(locale,self()),
    spawn(fun() -> register(remoto,self()),
                   locale!{remoto,ok},
                   receive {ok,_P} -> io:format("...\n") end

        end),
receive {remoto,ok} -> spawn(fun() -> remoto!{ok,self()} end)
end.
```

### whereis(Name)

In Erlang, quando usi la funzione **whereis(Name)** per cercare un processo registrato con un nome, *otterrai il PID di quel processo solo se la ricerca viene effettuata sullo stesso nodo dove il processo è registrato*. Se tenti di eseguire whereis(my_spreadsheet) su un nodo diverso da quello su cui il processo my_spreadsheet è stato registrato, otterrai undefined. Questo perché la funzione whereis/1 cerca il nome solo nella tabella dei nomi locali del nodo.

### global:whereis_name/1

Un'altra alternativa per situazioni dove i processi devono essere visibili globalmente è usare il modulo global, che registra i nomi dei processi in modo che siano accessibili da qualsiasi nodo nell'ambiente distribuito:

```console
% Su node1
global:register_name(my_global_spreadsheet, self()).

% Su node2 o qualsiasi altro nodo
global:whereis_name(my_global_spreadsheet).
```

Questa funzione restituirà il PID del processo my_global_spreadsheet indipendentemente dal nodo da cui viene chiamata, a condizione che il processo sia stato registrato con global:register_name/2 e che i nodi siano configurati correttamente per la comunicazione distribuita.

## global module

<https://www.erlang.org/doc/apps/kernel/global.html#content>

global
A global name registration facility.

This module consists of the following services:

Registration of global names
Global locks
Maintenance of the fully connected network

As of OTP 25, global will by default prevent overlapping partitions due to network issues by actively disconnecting from nodes that reports that they have lost connections to other nodes. This will cause fully connected partitions to form instead of leaving the network in a state with overlapping partitions.
These services are controlled through the process global_name_server that exists on every node. The global name server starts automatically when a node is started. With the term global is meant over a system consisting of many Erlang nodes.

The ability to globally register names is a central concept in the programming of distributed Erlang systems. In this module, the equivalent of the register/2 and whereis/1 BIFs (for local name registration) are provided, but for a network of Erlang nodes. A registered name is an alias for a process identifier (pid). The global name server monitors globally registered pids. If a process terminates, the name is also globally unregistered.

The registered names are stored in replica global name tables on every node. There is no central storage point. Thus, the translation of a name to a pid is fast, as it is always done locally. For any action resulting in a change to the global name table, all tables on other nodes are automatically updated.

Global locks have lock identities and are set on a specific resource. For example, the specified resource can be a pid. When a global lock is set, access to the locked resource is denied for all resources other than the lock requester.

Both the registration and lock services are atomic. All nodes involved in these actions have the same view of the information.

The global name server also performs the critical task of continuously monitoring changes in node configuration. If a node that runs a globally registered process goes down, the name is globally unregistered. To this end, the global name server subscribes to nodeup and nodedown messages sent from module net_kernel. Relevant Kernel application variables in this context are net_setuptime, net_ticktime, and dist_auto_connect.

The name server also maintains a fully connected network. For example, if node N1 connects to node N2 (which is already connected to N3), the global name servers on the nodes N1 and N3 ensure that also N1 and N3 are connected. In this case, the name registration service cannot be used, but the lock mechanism still works.

If the global name server fails to connect nodes (N1 and N3 in the example), a warning event is sent to the error logger. The presence of such an event does not exclude the nodes to connect later (you can, for example, try command rpc:call(N1, net_adm, ping, [N2]) in the Erlang shell), but it indicates a network problem.

## Record

records are a syntactic sugar over tuples, and their definitions (i.e., the layout and field names) aren't known to the Erlang runtime but to the Erlang compiler. If you're trying to use a record in the shell or in your code, and you get an error saying that the record name is undefined, it usually means that the record definition hasn't been loaded into the shell or hasn't been included correctly in your module.

### Load Record Definitions in the Shell

 When using the Erlang shell, you must ensure that the record definitions are available. You can do this by compiling the module that contains the record definitions with c/1 or by including the record definitions directly in the shell.

*rd(record_name, {field1, field2, field3}).*
Or, if the records are defined in a module, ensure to compile that module:

*c(module_with_records).*

Records are defined using the -record directive in Erlang modules

```console
-record(person, {name, age, gender}).
```

### Creating Record Instances

To create a record instance, you specify the record name followed by the fields in their respective order or by naming the fields explicitly:

*John = #person{name="John Doe", age=30, gender=male}.*
If you don't specify all the fields, the unspecified fields will be initialized to undefined.
You can access fields of a record using the record name and the field name, as follows:
*Name = John#person.name.*
To modify a record, you can use the record syntax to specify which fields to update:

*UpdatedJohn = John#person{age = 31}.*

### Limitations of Records

Scope: Record definitions are not global. They are scoped to the module in which they are defined unless included in other modules via header files.
Pattern Matching: While records improve readability, you need to remember that under the hood, they are just tuples. This means that operations like pattern matching are done using the tuple structure, which can complicate expressions slightly.

## Header Files

 Often, records are defined in header files (.hrl files) and included in multiple modules using -include directive. This approach promotes reuse and ensures consistency across different parts of an application.

*-include("person.hrl").*
When compiling the modules, make sure Erlang knows where to find the header file (spreadsheet.hrl). If the header file is in the same directory as your Erlang modules, it will be found automatically. Otherwise, you may need to specify the path during compilation:
*erlc -I path/to/headers module1.erl module2.erl*

### Loading Record Definition

If you have a header file, you can load the record definitions directly in the shell using the rr/1 or rr/2 function which reads record definitions from a file:

*rr("path/to/spreadsheet.hrl").*

## Mnesia

Using Mnesia, Erlang's built-in distributed database system, to save and load your spreadsheet record can provide several advantages including transaction support, fault tolerance, and straightforward integration with Erlang applications. Here’s how to set up and use Mnesia to manage your spreadsheet records:

Steps to Use Mnesia for Saving and Loading Spreadsheet Records

1. Define the Database Schema

You need to define a schema that includes a table for your spreadsheet records. Here’s how you can do it:

Start Mnesia and Create a Schema:

You need to start Mnesia with a directory where the database files will be stored.
erlang
Copia codice
mnesia:create_schema([node()]).
mnesia:start().
Create Tables:

Define a table to store your spreadsheet records. The attributes of the record become columns in the Mnesia table.
erlang
Copia codice
mnesia:create_table(spreadsheet, [
    {attributes, record_info(fields, spreadsheet)},
    {type, set},
    {record_name, spreadsheet}
]).

2.Save Records to Mnesia
To save a spreadsheet record into Mnesia, you typically use Mnesia's transaction or dirty functions. Here is how you can write a function to save records:

erlang
Copia codice
-module(spreadsheet_db).
-export([save_spreadsheet/1, load_spreadsheet/1]).

-record(spreadsheet, {name, owner, tabs, accesspolicy}).

save_spreadsheet(SpreadsheetRecord) ->
    Fun = fun() ->
        mnesia:write(SpreadsheetRecord)
    end,
    mnesia:transaction(Fun).
In the function save_spreadsheet/1, mnesia:write/1 is used to insert or update a record in the database. It's wrapped in a transaction to ensure consistency.

3.Load Records from Mnesia
Retrieving records from Mnesia can be done using a simple match or by key. Here’s a basic example to fetch a record by its key:

erlang
Copia codice
load_spreadsheet(Name) ->
    Fun = fun() ->
        mnesia:read({spreadsheet, Name})
    end,
    Result = mnesia:transaction(Fun),
    case Result of
        {atomic, [Record]} -> {ok, Record};
        {atomic, []} -> {error, not_found};
        {aborted, Reason} -> {error, Reason}
    end.
In load_spreadsheet/1, the record is fetched using its key, which in this case is assumed to be name.

4.Operational Considerations
Transactions: Mnesia transactions are used to ensure the atomicity and integrity of database operations. Transactions can also help in handling concurrency in a multi-user environment.
Starting Mnesia: Make sure that Mnesia is started with the appropriate parameters, and the schema is initialized before your application tries to access the database.
Error Handling: Always handle the possibility of transaction failures or other issues such as network failures or data corruption.
5. Advanced Features
Mnesia also supports more advanced features like:

Replication: You can easily configure Mnesia to replicate data across nodes for fault tolerance.
Complex Queries: Mnesia supports QLC (Query List Comprehensions) that can be used to perform complex queries on the data.
Conclusion
Using Mnesia to manage your spreadsheet records provides robust data management capabilities, especially useful in distributed Erlang applications. Mnesia’s integration within the Erlang runtime offers a seamless experience for managing persistent data without the need for external database systems. This setup is particularly beneficial for applications requiring high availability and data consistency across multiple nodes.
