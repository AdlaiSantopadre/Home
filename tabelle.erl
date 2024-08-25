-module(tabelle).
%OBBLIGATORIO SE VOGLIO USARE LA   fun2ms%
-include_lib("stdlib/include/ms_transform.hrl").

-export([init/0, prova_tabella/3, print_table/2, shared/2, populate/0, crea_agenda/1, filtraNazionalita/2, filtraProfessione/2, filtraNP/3, escludiNaz/2,filtraNazProf/3]).



init() ->

%creo una tabella con valori di default [],
% ovvero un set di tuple in cui il primo elemento deve essere univoco!!
% visibilità protected = tutti possono leggere la mia tabella
    MiaTab = ets:new(pippo,[]),
    %inserisco un elemento nella tabella
    ets:insert(MiaTab,{1,pippo,disney}),
    %inserisco un secondo elemento nella tabella
    ets:insert(MiaTab,{2,pippo,disney,5}),
    %le tuple possono avere forma e lunghezza diversa, l'unico requisito è che il primo
    %campo deve essere diverso/chiave

    Pid = self(),
    %
    spawn(fun() -> Pid!{ets:lookup(MiaTab,2)} end )
.

print_table(Tabella, Next) ->
    %recupero il prossimo elemento%
    Element = ets:next(Tabella,Next),   
    case Element of
        %se sono a fine tabella esco
        '$end_of_table' -> io:format("fine tabella .... \n"),
                            ok;
        _ ->io:format("iesimo elemento ~p\n",[Element]),
            print_table(Tabella,Element)  
    end
.

prova_tabella(N, Tipo, Nome) ->
   ets:new(Nome,[Tipo,named_table]),
    %popolo la mia tabella con numero -> "numero"%
    lists:foreach(fun(I)-> ets:insert(Nome,{I,integer_to_list(I)})end , lists:seq(1,N)),
    %first e next ritornano l'iesimo elemento di una tabella. E' diverso da lookup che ritorna la coppia
    First = ets:first(Nome),
    io:format("primo elemento ~p\n",[First]),    
    print_table(Nome, First) 
.
     

shared(Nome, Tipo) ->
       ets:new(Nome,[Tipo,named_table, public]),
       lists:foreach(fun(I)-> ets:insert(Nome,{I,integer_to_list(I)})end , lists:seq(1,10)).

 
populate() -> 
[
    {italia, claudio, prof},
    {italia, marco, prof},
    {italia, matteo, studente},
    {portogallo, vasco, prof},
    {portogallo, carla, prof},
    {italia, maria, collaboratrice},
    {germania, uwe, prof},
    {uk, iain, prof},
    {uk, irek, prof}
] . 


crea_agenda(Nome) -> 
    ets:new(Nome, [bag,named_table]),
    lists:foreach( fun(E) -> ets:insert(Nome,E) end , populate())
.

filtraNazionalita(N, Tabella) -> ets:match(Tabella,{N, '$2', '$3'} ).

filtraProfessione(P, Tabella) -> ets:match(Tabella,{'$1', '$2', P} ).

filtraNP(N,P, Tabella) -> ets:match(Tabella,{N, '$2', P} ).

escludiNaz(Tabella, N) -> ets:select(Tabella, [{{'$1', '$2', '$3'}, [{'/=','$1', N}], [['$2', '$1']] }]).

filtraNazProf(Tabella, N, P) ->
    MS = ets:fun2ms(fun({Nation, Name, Profession}) 
                        when (Nation== N), (Profession == P) -> [Name, Profession] 
                    end),
    ets:select(Tabella,MS).


%funzione che cancella tutte le entry la cui professione è = al parametro dato

deleteJob(T, J) -> ets:match_delete(T,{'$1','$2',J}).

%la select va vista come la SELECT di sql in cui selezionate i campi
%la match_obj va vista come la SELECT *
    
