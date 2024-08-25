-module(tel).
-export([create_tables/1, close_tables/0, add_usr/1, populate/0, restore_backup/0, disable_usr/1, loop_delete_disabled/1, delete_disabled/0	]).
-record(user, {

        phonenumber, %chiave primaria
        id,
        status = enabled,
        plan, % prepay | postpay
        services = [] % sms, call, data
}).


%noi abbiamo 3 tabelle
% 1 ets che gestisce i clienti,
% 1 dets che fa il bk della ets di sopra
% 1 ets che fa indexing phone id --> phone number

create_tables(FileName) ->
    ets:new(usrRam, [named_table, {keypos,#user.phonenumber}]),
    ets:new(usrIndex, [named_table]),
    dets:open_file(usrDisk, [{file,FileName}, {keypos,#user.phonenumber}]).

close_tables() ->
    ets:delete(usrRam),
    ets:delete(usrIndex),
    dets:close(usrDisk).


add_usr(#user{phonenumber = Phone, id = Id} = USR) ->
    ets:insert(usrIndex,{Id,Phone}),
    ets:insert(usrRam,USR),
    dets:insert(usrDisk,USR)
.



populate() ->
    Seq = lists:seq(1,1000),
    AddU = fun(Id) ->
                tel:add_usr(
                            #user{phonenumber = 7000 + Id,
                                    id = Id,
                                plan = prepay, services =[data,sms]})

            end,
    lists:foreach(AddU,Seq)
.

restore_backup() ->
    Insert = fun(#user{id = Id, phonenumber = Phone} = Usr) ->
              ets:insert(usrRam,Usr),
              ets:insert(usrIndex,{Id,Phone}),
            continue
            end,
    dets:traverse(usrDisk,Insert).


disable_usr(Id) ->
    [{_,Phone}] = ets:lookup(usrIndex,Id),
    io:format("~p --> ~p\n",[Id,Phone]),
    [OldUser] = ets:lookup(usrRam,Phone),
    add_usr(OldUser#user{status = disabled})
.


delete_disabled()->
    %è tipo una critical section
    % il flag messo a true garantisce che la funzione venga applicata 1 sola volta per riga
    ets:safe_fixtable(usrRam,true),
    loop_delete_disabled(ets:first(usrRam)),
    ets:safe_fixtable(usrRam,false)
.

loop_delete_disabled('$end_of_table') -> ok;

loop_delete_disabled(PhoneNumber) ->
    %Questa riga aveva un errore perche cercavamo di matchare una tupla con un singolo elemento con una tupla di N elemanti%
    %[{Usr}] = ets:lookup(usrRam, PhoneNumber),
    [Usr] = ets:lookup(usrRam, PhoneNumber),
    case Usr of
        #user{status=disabled, id = CustomID} ->
                                                io:format("cancello ....\n"),
                                                ets:delete(usrRam,PhoneNumber),
                                                ets:delete(usrIndex,CustomID),
                                                dets:delete(usrDisk,PhoneNumber);

        _ -> ok
    end,
loop_delete_disabled(ets:next(usrRam,PhoneNumber))
.
