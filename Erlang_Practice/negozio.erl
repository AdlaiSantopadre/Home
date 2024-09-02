-module(negozio).
-export([shop/1, startShop/0]).

%lo shop ha un DB fatto da una lista di tuple {frutto,quantità}
%frutto è un atomo

getCost(wallnut) -> 3;
getCost(banana) -> 5;
getCost(orange) -> 6.


shop(L) ->
  receive
    %la funzione add aggiunge nuova quantità al frutto F
    {add,F,Q} -> Newlist= lists:map(
    %%la funzione map "mappa" una lista in un'altra. La usiamo per creare il nuovo store
      fun({Fruit,Qt}) ->
        case (Fruit == F) of
          true -> {Fruit,Q+Qt} ;
          _ -> {Fruit,Qt}
        end
      end,L),
      shop(Newlist);
    {shop,Spesa,Pid} ->
      Costo = lists:foldr (
                fun({Frutto,Qt}, Acc)->
                  %selezioniamo solo il frutto che ci interessa
                  %filter ritorna SEMPRE una lista
                  [{F,Q}] = lists:filter( fun({F,_D}) -> F == Frutto end,L ),
                  case  Q >= Qt of
                      true -> getCost(F) * Qt + Acc;
                      _  -> 0
                  end
                end,
                0,Spesa),
                io:format("costo della spesa ~p è di ~p\n",[Spesa,Costo]),
                Pid!{costo,Costo},
                shop(L)
      ;
      {print} ->
          %foreach applica la stessa funzione ad ogni elemento della lista.
          %qui la usiamo per stampare lo store
          lists:foreach(fun({A,B}) -> io:format("Fruit: ~p; quantity: ~p\n",[A,B]) end, L),
          shop(L);

      {stop} -> io:format("shuting down the shop\n")
  end.

startShop() ->
  %?MODULE è una macro per il compilatore, verrà sostituita dal nome del modulo
  spawn(?MODULE,shop,[[{banana,10},{orange,5}]]).
