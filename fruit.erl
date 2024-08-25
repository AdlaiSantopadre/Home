-module(fruit).

-export([populate/0, costo/1, shop/1, init/0]).

costo(orange) -> 10;
costo(banana) -> 5;
costo(chestnut) -> 1.


populate() -> [{banana,10},{orange,5}].


shop(Store) ->

    receive 
        {checkout, Cart, Pid} -> %Cart è una lista di tuple {frutta,quantita} che il cliente vuole comprare%
                                % verificare prima che Cart può essere evaso e poi 
                                   %1) calcolare il costo della spesa
                                   %2) modificare lo store di conseguenza
                                   %3) tornare al cliente il prezzo da pagare

                                dummy_ok;

        {add, Fruit, Qt} -> %IsContained %;
                            %1) creo un filtro che mi torna una lista di tuple il cui primo elemento è = Fruit (ricevuto dal messaggio)%
                            %2) per assunzioni la lista è formata da 1 elemento o è vuota
                            %3) verifico che il risultato della filter non sia la lista vuota. In questo caso vuol dire che il frutto da aggiungere è già presente
                            %nel mio store e devo solo AGGIORNARE la quantità
                            IsContained = lists:filter(fun( {F,_Q})-> F == Fruit end, Store) =/= [],
                            case IsContained of
                                true -> MiaFun = fun(X)-> 
                                                    case X of
                                                        {Fruit, Y} -> {Fruit, Y + Qt} ; %aggiorno la quantità
                                                         _ -> X %identità
                                                    end       
                                                 end,
                                        NuovoStore = lists:map(MiaFun,Store),
                                        %chiamata ricorsiva con store aggiornato%                    
                                        fruit:shop(NuovoStore);
                                                 
                                false -> fruit:shop(Store ++ [{Fruit,Qt}])  %il frutto non è presente e lo appendo in coda alla lista                          
                            end;

        {print} ->  lists:foreach(fun({F,Q}) -> io:format("~p --> ~p \n",[F,Q]) end, Store), 
                    %chiamata ricorsiva con variabile Store non modificata%                    
                    shop(Store) ;		
        {stop} -> ok
    end
.

init() -> spawn(fruit, shop, [populate()]).
