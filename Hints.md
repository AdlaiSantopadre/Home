# FAQ

## whereis(Name)

In Erlang, quando usi la funzione **whereis(Name)** per cercare un processo registrato con un nome, *otterrai il PID di quel processo solo se la ricerca viene effettuata sullo stesso nodo dove il processo è registrato*. Se tenti di eseguire whereis(my_spreadsheet) su un nodo diverso da quello su cui il processo my_spreadsheet è stato registrato, otterrai undefined. Questo perché la funzione whereis/1 cerca il nome solo nella tabella dei nomi locali del nodo.

## net_adm:ping/1

Quando esegui net_adm:ping(node@localhost), stai inviando una richiesta di ping al nodo Erlang specificato.Se il nodo è raggiungibile e la comunicazione è stabilita correttamente, il nodo destinatario risponde:

- pong viene restituito se il nodo è raggiungibile e risponde correttamente. Questo indica che la connettività di rete tra i nodi è attiva e funzionante.
- pang viene restituito se il nodo non è raggiungibile o non risponde. Questo può accadere per vari motivi, come problemi di rete, configurazioni errate, o perché il nodo destinatario non è attivo.
