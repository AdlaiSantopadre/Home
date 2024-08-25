-module(seq).

-import(sub_module,[add/2]).
-export([somma/2, filtraPari/1, listaPariSmart/1, listaFilter/2,  anagrafica/0, votiAlti/1, votiAlti/2]).

somma(X,Y) -> 
    io:format("chiamato sottomodulo \n"),
    sub_module:add(X,Y).
%this part ends with a "." saying that there are no more parts of this function.
%definiamo una funzione che data una lista di interi mi da la lista dei pari%

filtraPari(X) ->
case X of
    [H | T] ->
        case (H rem 2) of
        0 -> [H] ++ filtraPari(T);
% Notice that this part ends with a semicolon ";" 
% that indicates that there is more of the function filtrapari to come.       
        1 -> filtraPari(T)        
        end;
    [] -> []
end
.

%assumiamo X sia una lista di interi
listaPariSmart(X)
-> lists:filter( fun(A) -> (A rem 2) == 0  end , X).
 
listaFilter(F,X) 
-> lists:filter( F , X).

%even(X) -> X rem 2 == 0.
%listaPariSmart2(X) -> lists:filter(even, X).


%anagrafica nome,eta degli studenti%
anagrafica() -> [
        {pippo, 30},
        {pluto, 24},
        {paperino, 19}
]
.

anagraficaCorsi() -> [
        {pippo, 30, programmazione},
        {pluto, 24,'IoT'},
        {pluto, 19,programmazione},
        {paperino, 19, logica},
        {paperino, 30, programmazione}
]
.

votiAlti(Voto) ->
 Anagrafica = [
        {pippo, 30},
        {pluto, 24},
        {paperino, 19}],
 Filtro = fun({_,Esito}) -> Esito > Voto end,
  lists:filter(Filtro,Anagrafica),
 io:format("sono qui\n")
.

votiAlti(Voto, Materia) -> 

 Filtro = fun({_,Esito,C}) -> (Esito > Voto) and (C == Materia)   end,
 Filtro1 = fun(X) -> case  X of
                      {_,E,Materia} -> E > Voto;
                        _ -> false                    
                    end
   end,
  PrimaLista = lists:filter(Filtro,anagraficaCorsi()),
  SecondaLista =lists:filter(Filtro1,anagraficaCorsi()),
  io:format("~p \n ------ \n ~p",[PrimaLista, SecondaLista]),
[PrimaLista,SecondaLista]
.
