
$shell1
erl -sname node1 -setcookie mycookie
c(spreadsheet).
spreadsheet:new(my_spreadsheet).
global:whereis_name(my_spreadsheet).
observer:start().

$shell2
erl -sname node2 -setcookie mycookie
% controllo la connettività tra i nosi
net_adm:ping(node1@DESKTOPQ2A2FL7).
%Arresto il processo
% non funziona dopo crash...{my_spreadsheet, node1@DESKTOPQ2A2FL7} ! stop.

## Gestione Access Policies

### creazione di tre processi

c(process_utility).
Names=[proc1,proc2,proc3].
process_utility:spawn_and_register_processes(Names).

%Proc1 = spawn(fun() -> receive stop -> ok end end).
%Proc2 = spawn(fun() -> receive stop -> ok end end).
%Proc3 = spawn(fun() -> receive stop -> ok end end).
% test Access policies
AccessPolicy = [{proc1, read}, {proc2, write}, {proc3, read}].
spreadsheet:share(my_spreadsheet,AccessPolicy).
spreadsheet:share(my_spreadsheet, [{proc2, write}, {proc3, write}]).
spreadsheet:share(my_spreadsheet, [{proc4, read}, {proc3, write}]).
spreadsheet:remove_policy(my_spreadsheet,proc1).
spreadsheet:info(my_spreadsheet).

# DISTRIBUTED ERLANG

# Avvio distributed_spreadsheet

erl -sname node1 -setcookie mysecretcookie

c(distributed_spreadsheet).

distributed_spreadsheet:new(my_dspreadsheet1).

observer:start().

global:whereis_name(my_dspreadsheet1).

distributed_spreadsheet:info(my_dspreadsheet1).

distributed_spreadsheet:share(my_dspreadsheet1,AccessPolicies).


## creazione di nodi di un cluster

erl -sname node1 -setcookie mysecretcookie
erl -sname node2 -setcookie mysecretcookie
erl -sname node3 -setcookie mysecretcookie
 or programmatically
erlang:set_cookie(node(), mysecretcookie).
erlang:set_cookie('node2@DESKTOPQ2A2FL7', mysecretcookie).

## test reassign_owner

NewOwnerPid = spawn(fun() -> receive after infinity -> ok end end).



## creazione di due/tre processi distribuiti

c(distributed_processes_utility).

net_adm:ping('node2@DESKTOPQ2A2FL7').
Processes = [{'proc1', 'node1@DESKTOPQ2A2FL7'},
             {'proc2', 'node2@DESKTOPQ2A2FL7'}
             %{'proc3', 'node3@DESKTOPQ2A2FL7'}
             ].
distributed_processes_utility:spawn_and_register(Processes).
AccessPolicies = [{self(), read}, {proc1, write}].

## Test scrittura / lettura

%% Integer value
distributed_spreadsheet:set(my_dspreadsheet1, 1, 2, 3, 42).

%% Atom value
distributed_spreadsheet:set(my_dspreadsheet1, 1, 2, 3, my_atom).

%% String (list) value
distributed_spreadsheet:set(my_dspreadsheet1, 1, 2, 3, "Hello").

%% Tuple value
distributed_spreadsheet:set(my_dspreadsheet1, 1, 2, 3, {ok, "Tuple"}).

%% Map value
distributed_spreadsheet:set(my_dspreadsheet1, 1, 2, 3, #{key => value}).

%% List of integers
distributed_spreadsheet:set(my_dspreadsheet1, 1, 2, 3, [1, 2, 3, 4]).
distributed_spreadsheet:set(my_dspreadsheet1, 1, 2, 3, "I'm a string").



distributed_spreadsheet:get(my_dspreadsheet1,1,2,3).
distributed_spreadsheet:get(my_dspreadsheet1,1,1,2,2000).
distributed_spreadsheet:get(my_dspreadsheet1,2,2,2).

## gestione persistenza

Tabs = [    [1, 2, 3],    [4, undef, "hello"],    [true, false, undefined]].

distributed_spreadsheet:to_csv("my_spreadsheet.csv",my_dspreadsheet1,5000).



