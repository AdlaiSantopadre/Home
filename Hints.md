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

## whereis(Name)

In Erlang, quando usi la funzione **whereis(Name)** per cercare un processo registrato con un nome, *otterrai il PID di quel processo solo se la ricerca viene effettuata sullo stesso nodo dove il processo è registrato*. Se tenti di eseguire whereis(my_spreadsheet) su un nodo diverso da quello su cui il processo my_spreadsheet è stato registrato, otterrai undefined. Questo perché la funzione whereis/1 cerca il nome solo nella tabella dei nomi locali del nodo.

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
``
## Uso di observer per Monitoraggio Grafico

Avvia Observer con il seguente comando nella shell Erlang:

```console
observer:start().
```

Usa Observer per vedere:

*Processi*: Puoi vedere tutti i processi in esecuzione, il loro PID, e lo stato (sospeso, attivo, ecc.).
Messaggi: Puoi vedere i messaggi inviati e ricevuti da ogni processo.
Supervisori: Se hai un supervisore per il foglio di calcolo, puoi visualizzare l'albero dei processi supervisionati.