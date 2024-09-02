-module(recordt).
-include_lib("stdlib/include/ms_transform.hrl").

-record(person, {name, surname, age, street=no_streed}).

-export([init/1, addPerson/4, trovaIndice/0, populate/2, find_person/2, find_age/2, init_food/2,populate_dets/1 , notItalian/1]).

addPerson(TableName, Name, Surname, Age)
-> ets:insert(TableName, #person{name=Name, surname=Surname, age=Age}).

populate(N, TableName) ->

    lists:foreach( fun(I)-> addPerson(TableName,"utente_"++integer_to_list(I),
                            "cognome_"++integer_to_list(I),
                            I + 10
                 ) end , lists:seq(1,N)).

%creiamo una tabella nominata di record person, in cui la chiave è il nome della persona
init(TableName) -> ets:new(TableName, [named_table, {keypos, #person.name}]). 

trovaIndice() -> #person.surname .


find_person(Tab,Name) -> ets:match_object(Tab,#person{name = Name, _='_'}).
%find_person(Tab,Name) -> ets:match_object(Tab,{Name,'$2','$3','$4'}).


find_age(Tab,Age)->
    MS = ets:fun2ms(fun(#person{name= N, age = A}) when A>= Age -> {N, A} end),
    ets:select(Tab,MS)
.


%creo una DETS con nome e path
init_food(Name, Path) ->
    dets:open_file(Name,[{type,bag},{file,Path}]).

populate_dets(Name)
->
    Food = [{italy,spaghetti}, {japan, ramen}, {italy, pizza}, {sweden, meatballs }],
    lists:foreach(fun(E) -> dets:insert(Name,E) end, Food)
.
    

notItalian(Tab) ->
    MS = ets:fun2ms(fun({Nation, Food}) when Nation /= italy -> Food end),
    dets:select(Tab,MS).


