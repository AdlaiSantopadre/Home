-module(prova_record).
%includiamo il file di definizione del record 
-include("record.hrl").

-export([crea_persona/4, crea_corso/5,populate/1, creaDB/0, select/2, print_prova/1, print_corso/1, print_corsoNestet/1]).

%semplice funzione che mi crea una lista di 10 record usando
%il contatore X come nome/cognome e per l'età
populate(Corso) -> 
lists:map( fun(X) -> crea_corso(Corso,X,X,18+X, nostreet) end , lists:seq(1,10))
. 

creaDB()  -> populate(adcc) ++ populate(iot).

crea_persona(Name,Surname,Age,Street) -> 
    #person{name = Name, surname = Surname, age = Age, street = Street}.



crea_corso(NomeCorso, Name,Surname,Age,Street)-> 
    #corso{nome=NomeCorso, 
      studente= crea_persona(Name,Surname,Age,Street)
    }
.



%funzione che mi selezione tutti gli studenti di un dato corso
%con un'età (voto) > di un certo parametro

select(NomeCorso, E)-> lists:filter( 

                fun(X) -> case X of 
                               #corso{nome=NomeCorso, studente = Record} -> 
                                   case Record of
                                        #person{name =_, surname=_, age = A, street = _ }  -> A >= E                            
                                    end;
                                  
                               _ -> false
                            end
                end , 
creaDB()).


%una funzione che prende un record di tipo prova e stampa il contenuto a video
%diciamo alla funzione che il parametro è un record di tipo #prova in cui leghiamo
%i due campi con le variabili P ed S
print_prova(#prova{primo = P, secondo = S}) -> 
    
    io:format("primo campo = ~p , secondo campo =~p \n", [P,S]).


%il record più interno deve essere passato come una tupla di n+1 elementi il cui primo elemento è l'atomo che da il tipo
print_corso(#corso{ nome = C, studente = {person, _N, _S, _A, _Str} }) -> 
    io:format("~p ~p ~p ~p \n",[_N, _S, _A, _Str]).


 

