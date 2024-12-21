-module(shop_db).
-include_lib("stdlib/include/qlc.hrl").

-record(shop,{item,quantity,cost}).
-record(cost,{name,price}).


-export([init_once/0, start/0,populate/0, print_fruit/1, remove_fruit/1, delete_all/2, add_more/2, print_all/2, select_all_shop/0, select_all_smart/0]).

init_once() ->
        mnesia:create_schema([node()]),
        mnesia:start(),
        mnesia:create_table(shop,[{attributes, record_info(fields, shop)}]),
        mnesia:create_table(cost, [{attributes, record_info(fields,cost)}]),
        mnesia:stop().


start() ->
    mnesia:start(),
    mnesia:wait_for_tables([shop,cost], 20000).

populate() ->

        Myshop = [#shop{item= banana, quantity = 100,  cost =0.1},
                  #shop{item= orange, quantity = 200,  cost =0.3},
                  #shop{item= peach, quantity = 300,  cost =0.5},
                  #shop{item= passion_fruit, quantity = 50,  cost =1}
        ],

        Mycost = [#cost{name=banana,price=1},
                    #cost{name=orange,price=0.5},
                #cost{name=peach,price=1},
                #cost{name=passion_fruit,price=2}
    ],

    %una transazione è una funzione lambda del tipo fun()-> F end%
    InsertValue = fun() ->  
                        lists:foreach(fun(Rcd) -> mnesia:write(Rcd) end ,
                        Myshop ++ Mycost)                  
                    end,
    mnesia:transaction(InsertValue)
.
      

print_fruit(Fruit) ->

    %Oid = nome tabella, chiave primaria
    Oid = {shop,Fruit},
    {_,Ret} = mnesia:transaction( fun()-> mnesia:read(Oid) end ),
    Ret
.


remove_fruit(Fruit) ->
    Oid = {shop,Fruit},
    mnesia:transaction( fun()-> mnesia:delete(Oid) end )
  .


delete_all(L, TableName) ->
%L è una lista di chiavi
%mi creo una lista di Oid
    %itera mi crea una lista di [OID] = [{tabella,chiave}]
    Itera = lists:map( fun(F) -> {TableName,F} end, L),
    io:format("cancello ~p", [Itera]),
    F = fun() -> lists:foreach(fun(X) -> mnesia:delete(X) end, Itera) end, 
    mnesia:transaction(F)
.



add_more(Frutto, Qt) ->
    F =     fun() ->
                    Oid={shop,Frutto},
                    [Entry] = mnesia:read(Oid),
                    OldQ = Entry#shop.quantity, 
                    NewFruit = Entry#shop{quantity = OldQ+Qt},                       
                    mnesia:write(NewFruit)
            end,

    mnesia:transaction(F).
               
    
%select shop.* from shop, cost where shop.item = cost.name and cost.price < Price
print_all(Nro, Price) ->

    Q = qlc:q([ X#shop.item || X <-mnesia:table(shop),
                    X#shop.quantity < Nro,
                    Y <- mnesia:table(cost),
                    %condizione di join
                    Y#cost.name =:= X#shop.item,                    
                    Y#cost.price < Price
                     ]),
    F = fun() -> qlc:e(Q) end,
    mnesia:transaction(F)
.


select_all_shop() ->
    Print = fun(#shop{item=I, quantity=Q, cost=C}, Acc) ->
            %io:format(" fruit:  ~p , quantity:  ~p, cost:  ~p \n",[I,Q,C])   
            Acc ++ [ {I,Q,C}] 
        end,
        Tran= fun() -> mnesia:foldr(Print,[],shop) end,
        {_,Res} = mnesia:transaction(Tran),
        lists:foreach(
                        fun({I,Q,C}) ->
                            io:format(" fruit:  ~p , quantity:  ~p, cost:  ~p \n",[I,Q,C]) 
                        end,   
            Res)
.

select_all_smart() ->

        Q = qlc:q([ X || X <-mnesia:table(shop)]),
        F = fun() -> qlc:e(Q) end,
        {_,Res} = mnesia:transaction(F),   
        lists:foreach(
                        fun({_,I,Qt,C}) ->
                            io:format(" fruit:  ~p , quantity:  ~p, cost:  ~p \n",[I,Qt,C]) 
                        end,   
            Res)
.
