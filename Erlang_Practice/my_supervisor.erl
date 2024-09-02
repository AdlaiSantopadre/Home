-module(my_supervisor).
-export([start_link/2, stop/1]).
-export([init/1]).

start_link(Name, ChildSpecList) ->
  register(Name, spawn_link(my_supervisor, init, [ChildSpecList])),
  ok.

init(ChildSpecList) ->
  process_flag(trap_exit, true),
  loop(start_children(ChildSpecList)).

start_children([]) -> [];
start_children([{M, F, A} | ChildSpecList]) ->
  %apply executes a function F from module M with parameter A %
  %similar to M:F(A)
  %catch allows to cacth an error, e.g., the module or function does not exists
  case (catch apply(M,F,A)) of
    {ok, Pid} -> [{Pid, {M,F,A}}|start_children(ChildSpecList)];
    _ -> start_children(ChildSpecList)
end.


%ChildList is a list of tuples of this format {Pid,{M,F,A}}
%so it can be seen as an hashtable PID -> {M,F,A}
%that is PID -> executed function

restart_child(Pid, ChildList) ->
    {value, {Pid, {M,F,A}}} = lists:keysearch(Pid, 1, ChildList),
    {ok, NewPid} = apply(M,F,A),
    [{NewPid, {M,F,A}}|lists:keydelete(Pid,1,ChildList)]
.
loop(ChildList) ->
  receive
    %whenever the supervisor receive and exit and a PID it restert
    %the function executed by PID with a new one
    {'EXIT', Pid, _Reason} ->
        NewChildList = restart_child(Pid, ChildList), loop(NewChildList);
    {stop, From} ->
        From ! {reply, terminate(ChildList)}
  end
.

stop(Name) ->
  Name ! {stop, self()},
  receive {reply, Reply} -> Reply end
.

terminate([{Pid, _} | ChildList]) ->
  exit(Pid, kill),
  terminate(ChildList);
terminate(_ChildList) -> ok.
