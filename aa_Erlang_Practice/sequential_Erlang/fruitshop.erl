
-module(fruitshop).

-export([populate/0]).
-export([shop/1]).
-export([init/0]).

cost(arancia) -> 10;
cost(banana) -> 5;
cost(castagna) -> 1.

populate() -> [{banana,10},{arancia,5},{castagna,20}].
shop(Store) ->
    receive
        {checkout,Cart,Pid} -> 
        Costo = lists:foldr(
            fun({Frutto,Qt},Acc) -> 
                            [{F,Q}] =lists:filter(fun({F,_D}) -> F == Frutto end,Store),
                            case Q >=Qt of
                                true -> cost(F)*Qt + Acc;
                                _ -> 0
                            
                                    
                            end
            end,       
                    0,Cart)
       ; 
        {add,Fruit,Qt} ->  %the quantity Q has to be added to the fruit F in the store
         %IsContained è true se la lista ritornata da filter è non vuota
        IsContained = lists:filter(fun( {F,_Q})-> F == Fruit end, Store) =/= [],
        case IsContained of
                true   -> AddFruitQt = fun(X) ->
                %domanda , dove viene presa X?
                                    case X of
                                    
                                      {Fruit, Y} -> {Fruit, Y + Qt};
                                          _ -> X
                                    end 
                                        end,
                          NuovoStore = lists:map(AddFruitQt,Store), 
                          shop(NuovoStore);                     
                false -> shop(Store ++ [{Fruit,Qt}]) 
           end ;    
   
                
          
               
                
           
        %shop(NewList); 

        %{shop,Spesa,Pid} -> _
        %                   - se spesa è [] ritorna Costo
        %%                  - altrimenti prendi primo elemento
        %                         - se F è in L e la sua quantità e sufficiente
        %                            calcola Nuovototale e passa al resto degli elementi
        %                            se F non è in L chiama stop
                            
        
        % ;
        {print} -> 
                lists:foreach(fun({F,Q}) -> io:format("Fruit :~p; quantity ~p\n",[F,Q])end, Store) , 
                                 
                shop(Store)% la chiamata ricorsiva a shop serve a non far terminare l`attore

         ;
        {stop} -> ok
    end.
init() -> spawn(fruitshop, shop, [populate()]).