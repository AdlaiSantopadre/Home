%intestazione del file per creare un record

%per usare i record da shell bisogna precarirare la definizione
%con la direttiva rr("file_definizione.hrl")

-record(person, {name, surname, age, street}).

-record(person1, {name, surname, age=18, street = no_steet}).

-record(corso, {nome, studente=#person{}}).

%per ottenere la lista dei campi di un record posso usare la
%funzione record_info(field, tipo_del_record)

-record(prova,{primo,secondo}).
