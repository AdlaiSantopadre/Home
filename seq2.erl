
-module(seq2).

-export([filtra/2, filtra2/2, quadrato/1, mioFiltro/1, sommaLista/1, lunghezza/1, invertiLista/1, stampaLista/1, creaN/2, pow/1 ]).

%usando la list comprehension creo una funzione filtro%
filtra(L,F) -> [ X || X <- L , F(X)] .

%invece di usare la comprehension uso direttamente la filter del modulo lists%
filtra2(L,F) -> lists:filter(F, L). 


mioFiltro(X) -> X > 3.

%mi creo una funzione che ritorna il quadrato dei primi N numeri%

quadrato(N) -> [X*X ||  X <- lists:seq(0,N)] .


%sommiamo gli elementi di una lista con la funzione foldl%

sommaLista(L) -> 
    Somma = fun(I, Acc) -> I + Acc end ,
%   lists:foldl(fun(I, Acc) -> I + Acc end, 0, L).
    lists:foldl(Somma, 0, L).


%calcolo la lunghezza di una lista con fold

lunghezza(L) -> 
    MiaFun = fun( _, Acc) -> Acc +1 end,
    lists:foldl(MiaFun,0,L).


invertiLista(L) ->

    MiaFun = fun ( I, Acc) -> Acc ++ [I] end,
    lists:foldr( MiaFun, [], L) .


stampaLista(L) ->
    lists:foreach( fun(X) -> io:format(" ~p -- ~p  \n", [X, ciao]) end, L).


%funzione che crea N attori che mi mandano un ack%
creaN(Msg, N) ->
    %creo una lista di N numeri su cui iterare%
    Iteratore = lists:seq(1,N),
    % mi salvo il pid della console
    PidConsole = self(),
    %funzione che viene usata dalla foreach
    Attore = fun(I) -> 
                        spawn(fun()-> PidConsole!{Msg,I,self()} end)     
             end,
    lists:foreach(Attore,Iteratore) 
.


        
%data una lista mi creo una lista delle potenze%
pow(L) -> lists:map(fun(X) -> X*X end , L).    

