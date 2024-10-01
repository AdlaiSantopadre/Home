# Test

## Sequenza iniziale comandi da shell

%compilare spreadsheet % Creare un foglio con nome "my_spreadsheet" e dimensioni predefinite % Verificare che il processo sia terminato
c(spreadsheet).
spreadsheet:new(my_spreadsheet).
global:whereis_name(my_spreadsheet).

my_spreadsheet ! stop.% Fermare il processo

## Test scrittura / lettura

spreadsheet:set(my_spreadsheet,2,2,2,"prova").
spreadsheet:set(my_spreadsheet,2,2,2,"prova",2000).

spreadsheet:get(my_spreadsheet,1,1,2).
spreadsheet:get(my_spreadsheet,1,1,2,2000).
spreadsheet:get(my_spreadsheet,2,2,2).

observer:start().

## gestione persistenza

Tabs = [    [1, 2, 3],    [4, undef, "hello"],    [true, false, undefined]].
spreadsheet:to_csv("my_spreadsheet.csv",my_spreadsheet).
spreadsheet:from_csv("my_spreadsheet.csv").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Avvio con due shell

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

$shell1
spreadsheet:info(my_spreadsheet).
spreadsheet:share(my_spreadsheet,[{node1,write},{node2,read}]).

c(spreadsheet).
{ok, Pid} = spreadsheet:new(my_spreadsheet, 10, 10, 1).

## gestione Access Policies

## creazione di tre processi

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

## *distributed_spreadsheet*

c(distributed_spreadsheet).
distributed_spreadsheet:new(my_dspreadsheet1).
observer:start().
global:whereis_name(my_dspreadsheet1).
distributed_spreadsheet:info(my_dspreadsheet1).


