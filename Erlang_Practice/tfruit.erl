-module(tfruit).

-export([init/2, start/2, loop/1, popolate/0]). 

-include_lib("stdlib/include/ms_transform.hrl").

%la mia tabella ha la forma {Frutta,Quantità,Prezzo}


loop(TableName) ->
    receive 
        {print, Pid} -> %ritorno tutte le righe della mia tabella
                        Pid!{ets:match_object(TableName,{'$1', '$2','$3'})},
                        loop(TableName);
        {price,FruitType,Pid} -> Riga = ets:lookup(TableName,FruitType),
                                case Riga of
                                    [] -> Pid!{fruit_not_present} ;
                                    [H | _] -> {_,_,Prezzo} = H,
                                                Pid!{Prezzo} 
                                end,
                                loop(TableName);
        {add,Frutto, Pid} -> 
                                {FruitType,Qnt,Pr} = Frutto,
                                Riga = ets:lookup(TableName,FruitType),
                                %controllo che il frutto sia presente                                
                                case Riga of
                                    [] ->      ets:insert(TableName, Frutto) ;
                                    [H | _] -> {FT,Qt,Pr} = H,
                                                ets:insert(TableName,{FT,Qt+Qnt,Pr})
                                end,
                                Pid!{fruit_added},
                                loop(TableName);
        {delete,Fruit, Pid} -> Flag = ets:match_delete(TableName, {Fruit,'$2','$3'}),
                                Pid!{deleted,Flag},
                                loop(TableName);
        {total,Pid}     -> %calcolo il valore totale del mio magazzino
                            Totale = ets:foldl(fun({_,Q,P}, Acc) ->   Q*P + Acc end ,0,TableName),
                            Pid!{total,Totale},
                            loop(TableName);

 %       {shop, ListaSpesa, Pid} -> %1) lista spesa è una lista di coppie Frutto,Qt
                                   %2) Controllo che la spesa può essere evasa
                                   %3) Evado la spesa e spedisco il conto a Pid   
                                
        {stop} ->           %salvo la tabella su file
                            ets:tab2file(TableName, "miodump"),
                            ets:delete(TableName),           
                            ok
    end
.

popolate() -> [
    {banana,100,2},
    {orange,50,2},
    {apple,10,1},
    {wallnuts,400,4}
]. 

init(TableName, ServerName) -> 
    %registro il server col nome dato%
    register(ServerName,self()),

    {Flag, _} = ets:file2tab("miodump"),

    case Flag of
        error -> %creo la tabella a la popolo
                 ets:new(TableName, [named_table,private]),
                 lists:foreach(fun(I) -> ets:insert(TableName, I) end , popolate());
        ok -> ok
    end,
    loop(TableName)
.

%comando da shell per avviare il tutto
start(TableName, ServerName) -> 
    spawn( tfruit, init,[TableName, ServerName]).

