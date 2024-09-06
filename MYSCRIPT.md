
c(spreadsheet).
% Creare un foglio con nome "my_spreadsheet" e dimensioni predefinite
spreadsheet:new(my_spreadsheet).
% Controllare il processo registrato
whereis(my_spreadsheet).
% Fermare il processo
my_spreadsheet ! stop.
% Verificare che il processo sia terminato
whereis(my_spreadsheet).

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
share(my_spreadsheet,[{my_spreadsheet,write},{node2,read}]).