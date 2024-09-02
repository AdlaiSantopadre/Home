-module(resilient).

-export([startCentralina/1, cellLoop/3,getRequestFrequency/2, init/1, exited/3]).


getFrequencies() -> [10,11,12,13,14,15].

startCentralina(NameServer)
-> register(NameServer,spawn(?MODULE, init, [NameServer] ))
.

init(NameServer) ->
  process_flag(trap_exit,true),
  cellLoop(getFrequencies(),#{},NameServer)

.

%qui definisco getFree come insieme di clausole%
getFree([]) -> {error, nofrequence};
getFree([H | _T]) -> io:format("alloco ~p\n",[H]),{ok, H}.


%Freq e' una lista di frequenze libere (interi)
% L e' una MAPPA #{Pid => Frequenza}
cellLoop(Freq, L, NameServer)->
  receive
    {request,Pid, allocate} ->
                            X = maps:find(Pid,L),
                            case X of
                              {ok,_F} -> Pid!{error,alreadyFreq},cellLoop(Freq, L,NameServer);
                              error -> ok
                            end,
                            {M,F} = getFree(Freq),
                            case M of
                              %stabilisco un link tra client e server%
                              ok ->
                                  link(Pid),
                                  Pid!{ok,F},
                                  cellLoop(lists:delete(F,Freq),maps:put(Pid,F,L),NameServer);
                              error -> Pid!{error, nofreq},
                                  cellLoop(Freq, L,NameServer)
                            end;
      %assumiamo che un PID abbia SOLO una frequenza%
    {request,Pid, deallocate} -> X = maps:find(Pid,L),
                                case X of
                                  {ok,Frequenza} ->
                                          Pid!{ok, deallocated},
                                          cellLoop(Freq++[Frequenza],maps:remove(Pid,L),NameServer);

                                  error -> Pid!{error, noallocatedfreq}, cellLoop(Freq,L,NameServer)
                                end;
    {list} -> io:format("le frequenze libere sono: ~p\n",[Freq]),
              cellLoop(Freq,L,NameServer);
    {'EXIT',Pid,_Reason}
      -> io:format("process ~p DIED \n",[Pid]),
        {F,Lst} = exited(Freq,L,Pid),
        cellLoop(F,Lst,NameServer);

    {stop} -> unregister(NameServer)
    end
  .

exited(_Freq,_L,Pid) ->
  io:format("chiamato exited con pid ~p\n",[Pid]),
  {_Freq,_L}
  .

getRequestFrequency(Pid, Server) ->
  Server!{request,Pid,allocate}
.
