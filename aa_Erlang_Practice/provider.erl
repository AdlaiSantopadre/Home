-module(provider). %% è il mio backend che racchiuderò in un modulo server

%per provare il record da shell usare rd(usr, {num, id, status = enabled, plan, service=[]}))
%plan = prepaid | flat
%status = enabled | disabled
-record(usr, {num, id, status = enabled, plan, service=[]}).

-include_lib("stdlib/include/ms_transform.hrl").

-export([create_tables/0, close_tables/0, update_user/1, add_user/1, add_userComplicato/1, restore_tables/0, populate/1, disable_number/1, disable_user/1, delete_disabled/0, delete_loop_disabled/1,
delete_smart/0]).

%utilizziamo 3 tabelle
%una ets che contiene i record degli utenti
%una dets che mi fa il backup dei miei utenti
%una ets che mi mantiene il mapping telefono -- utente

create_tables() ->
    ets:new(userRam, [named_table, {keypos, #usr.num}]),
    %tabella ets che prende una tupla numero -- id 
    ets:new(userIndex,[named_table]),
    %creo la tabella su disco che è il backup della mia tabella utenti
    dets:open_file(userDisk, [{file, "utenti"}, {keypos, #usr.num}])
.

close_tables() ->
    ets:delete(userRam),
    ets:delete(userIndex),
    %chiudo il file della tabella disco, mentre quelle in memoria vengono CANCELLATE
    dets:close(userDisk)
.

update_user(Usr) -> 
    ets:insert(userRam,Usr),
    dets:insert(userDisk, Usr)
 .

add_userComplicato(User) ->
    case User of
        #usr{ num = Num, id= ID,status = _, plan = _, service=_ } -> 
             ets:insert(userIndex, {ID,Num} ),
             update_user(User) ;
        _ -> err
    end    
.

add_user(#usr{num=Num, id = ID} = User) ->
    ets:insert(userIndex, {ID, Num} ),
    update_user(User) 
.

populate(N) ->
    lists:foreach( fun(I)-> add_user(#usr{num=5000+I, id = I, 
                    plan = case I rem 3 of 0 -> prepayed ; _ -> flat end }) end , lists:seq(1,N))
.
restore_tables() ->
    Insert = fun(#usr{num=Num, id = ID} = User) ->
                ets:insert(userIndex,{ID,Num}),
                ets:insert(userRam,User),
                %necessario per far continuare la funzione, come da specifica
                continue             
            end,
    %simile alla fold applica la funzione ad ogni riga della tabella file
    dets:traverse(userDisk, Insert)
.

%dato un numero mi disabilita la linea
disable_number(Number) ->
    Save = ets:lookup(userRam,Number),
    case Save of
        [] -> user_undefined    ;
        [H | _T ] ->   
               
                    NewRecord = #usr{ num = H#usr.num,
                                      id = H#usr.id, 
                                      status = disabled,
                                      plan = H#usr.plan, 
                                      service= H#usr.service          
                                },
                    update_user(NewRecord)
    end
.  
 
%dato un utente me lo cancella

disable_user(UserId) ->
        User = ets:lookup(userIndex, UserId),
        case User of 
            [] -> user_not_found;
            [H| _T] -> {_,N} = H,
                    disable_number(N)
        end
.


%cancello tutte le utenze che sono disabilitate
delete_disabled()
->
    ets:safe_fixtable(userRam, true), 
    %iteriamo sui numeri di telefono
    delete_loop_disabled(ets:first(userRam)),
    ets:safe_fixtable(userRam, false)
.

delete_loop_disabled('$end_of_table') -> ok;

delete_loop_disabled(Number) -> 
    io:format("chiamto con id ~p \n", [Number]),
    case ets:lookup(userRam, Number) of
        [#usr{id=ID, status=disabled} | _T] -> 
                                        io:format("itero numero ~p ed id  ~p \n", [Number,ID]),
                                        ets:delete(userRam,Number),
                                        ets:delete(userIndex,ID),
                                        dets:delete(userDisk,Number);
        _ -> ok 
    end,
    delete_loop_disabled(ets:next(userRam,Number))                                           
.


%funzione smart che usa la select invece della traverse
delete_smart() ->
    MS = ets:fun2ms(fun(#usr{id = ID, num = Num , status=disabled}) -> {ID, Num} end),
    ToDelete = ets:select(userRam,MS),%seleziono tutti gli utenti della query MS (matching specification) su status=disabled. ToDelete ritorna una lista di coppie{ID,Num}
    lists:foreach(fun({ID, N  }) -> ets:delete(userRam,N), ets:delete(userIndex,ID), dets:delete(userDisk,N), io:format("cancello utente ~p con numero ~p \n", [ID,N]) end , ToDelete) %delete ha sempre successo
.
 print-user(Num) -> ets:lookup(Arg1, Arg2);
 
 print_user(ID)  -> Ret = ets:lokup(usrindex,ID),
        -> case Ret of
             ->
                
        end


% creare un server loop che mi risponda ai messaggi con spawn/4



