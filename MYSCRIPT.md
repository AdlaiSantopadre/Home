# Sequenza comandi da shell

c(spreadsheet).  %compilare spreadsheet

spreadsheet:new(my_spreadsheet).  % Creare un foglio con nome "my_spreadsheet" e dimensioni predefinite   % Controllare il processo registrato

my_spreadsheet ! stop.% Fermare il processo

whereis(my_spreadsheet).  % Verificare che il processo sia terminato
% crea due processi
ProcA = spawn(fun() -> receive stop -> ok end end).
ProcB = spawn(fun() -> receive stop -> ok end end).
ProcA.
ProcB.
spreadsheet:share(my_spreadsheet,[{ProcA, read}, {ProcB, write}]).
spreadsheet:remove_policy(my_spreadsheet,ProcA).

## gestione persistenza

Tabs = [    [1, 2, 3],    [4, undef, "hello"],    [true, false, undefined]].
spreadsheet:to_csv("my_spreadsheet.csv", #spreadsheet).

Spreadsheet = spreadsheet:from_csv("my_spreadsheet.csv").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Avvio con due shell
$shell1
erl -sname node1 -setcookie mycookie
c(spreadsheet).
spreadsheet:new(my_spreadsheet).

$shell2
erl -sname node2 -setcookie mycookie
% controllo la connettività tra i nosi
net_adm:ping(node1@DESKTOPQ2A2FL7).
%Arresto il processo
{my_spreadsheet, node1@DESKTOPQ2A2FL7} ! stop.
%shell1
% Verificare che il processo sia terminato
whereis(my_spreadsheet).

$shell1
spreadsheet:share(my_spreadsheet,[{my_spreadsheet,write},{node2,read}]).

c(spreadsheet).
{ok, Pid} = spreadsheet:new(my_spreadsheet, 10, 10, 1).
