# Hints

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

## loop

Il ciclo di vita di un processo in Erlang è gestito dalla funzione loop/1. La funzione loop/1 è progettata per ricevere e gestire messaggi in modo continuo, mantenendo lo stato aggiornato del processo. Se il processo termina lo stato non sarà più mantenuto e non sarà possibile inviare ulteriori richieste al processo.
