-module(demo).

-export([double/1, uno/0, fib/1, fibC/1, area/2,  sumList/1, sumGlobal/1, invList/1] ).


double(V) -> V * V .

uno() -> 1.


fib(X) ->
    if 
        X < 0 -> err;
        X == 0 -> 1;
        X == 1 -> 1;
        X > 1 -> fib(X-1) + fib(X-2)
       
    end
.

fibC(X) when X<0 -> err;
fibC(X) when (X>=0) and (X<2)-> 1;
fibC(1) -> 1;
fibC(X) -> fibC(X-1)+fibC(X-2).


area (cerchio, [X | _]) -> X * X * 3.14;
area (quadrato, X) -> 
        [Lato | _ ] = X, 
        Lato * Lato ;

area (rettangolo, X) -> 
    [Base | Coda ] = X, 
    [Altezza | _] = Coda, 
    Base * Altezza ;
area (_, _) -> not_implemented. 


sumList([]) -> 0;
sumList([H | T]) -> H + sumList(T).


sumGlobal(X) ->
    case X of
        [] -> 0 ;
        [H | T ] -> H + sumGlobal(T)    
    end
.
invList([H|T])->
    invList(T) ++ [H] ;
    invList([]) -> [].
    

