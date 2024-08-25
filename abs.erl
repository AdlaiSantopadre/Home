-module(abs).
-export([insert/2]).
-export([sum/1]).
-export([empty/0]).
-export([test/0]).
-export([battery/1]).
%usiamo tuple per implemente]are alberi binari
empty()-> {}.

sum({}) -> 0;
sum({L,V,R}) -> sum(L) + sum(R) + V.

insert({},Val) -> {{},Val,{}};
insert ({L,V,R},Val) -> 
    case Val >= V of
          true -> {L,V,insert(R,Val)};
          false -> {insert(L,Val),V,R}
          end.
%test è una tripla {nome, funzione, valore atteso)}
find({{},V,{}},Val) -> V==Val;
find()->({L,Val,R},Val) -> 
    case Val >= V of
          true -> find(R,Val);
          false -> find(L,Val)
          end
          .
test() ->
[
    {test1, insert(empty(),5) == {{{},5,{}}}},
    {test2, insert({{{},5,{}}},6) == {{},5,{{},6,{}}}}
]
.
%mi creo una serie di unit test
test_insert() -> insert
battery(L) -> lists:map(fun(F) -> abs:F() end , L).
%batteria con tripletta Modulo, Funzione,Parametri ,ValoreAtteso
concurrentBattery()  ->
            Master =self(),
        Spawn = fun(F) -> spawn(fun() ->Master! { F,abs:f()} end) end,
        PidList = lists:map(Spawn,L)
        lists: foreach ( fun(_) -> 
            receive {F,M} -> io:format("test: ~p, res ~p\n",[F,M]) end end ,PidList).
        .