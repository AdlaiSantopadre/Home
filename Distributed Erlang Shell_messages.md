# MESSAGGI DA SHELL

## First DISTRIBUTED VERSION spreadsheet.erl

*$shell1*
erl -sname node1 -setcookie mycookie
c(spreadsheet).
spreadsheet:new(my_spreadsheet).
global:whereis_name(my_spreadsheet).
observer:start().

*$shell2*
erl -sname node2 -setcookie mycookie
% controllo la connettività tra i nodi
net_adm:ping(node1@DESKTOPQ2A2FL7).
*%Arresto il processo
% non funziona dopo crash...{my_spreadsheet, node1@DESKTOPQ2A2FL7} ! stop.*

## Gestione Access Policies con creazione di tre processi

c(process_utility).
Names=[creator,user2,user3].
process_utility:spawn_and_register_processes(Names).

AccessPolicy = [{proc1, read}, {proc2, write}, {proc3, read}].
spreadsheet:share(my_spreadsheet,AccessPolicy).
spreadsheet:share(my_spreadsheet, [{proc2, write}, {proc3, write}]).
spreadsheet:share(my_spreadsheet, [{proc4, read}, {proc3, write}]).
spreadsheet:remove_policy(my_spreadsheet,proc1).
spreadsheet:info(my_spreadsheet).

## Improved DISTRIBUTED ERLANG distributed_spreadsheet.erl con Schema gen_server

## creazione di nodi di un cluster

erl -sname node1 -setcookie mysecretcookie
erl -sname node2 -setcookie mysecretcookie
erl -sname node3 -setcookie mysecretcookie

net_adm:ping('node2@DESKTOPQ2A2FL7').

* or programmatically*
erlang:set_cookie(node(), mysecretcookie).
erlang:set_cookie('node2@DESKTOPQ2A2FL7', mysecretcookie).

## Avvio Gen_server

c(distributed_spreadsheet).
distributed_spreadsheet:new(my_spreadsheet).
observer:start().
global:whereis_name(my_spreadsheet).
distributed_spreadsheet:info(my_spreadsheet).
distributed_spreadsheet:stop(my_spreadsheet). % without Name  the gen_server crashes

## creazione di due/tre processi distribuiti

**da ripetere su ogni nodo**
c(distributed_processes_utility).
NamesAndNodes = [{{my_spreadsheet,owner}, 'node1@DESKTOPQ2A2FL7'},
                  {'user2', 'node2@DESKTOPQ2A2FL7'},
                  {'user3', 'node3@DESKTOPQ2A2FL7'}
                                       ].
distributed_processes_utility:register_self(NamesAndNodes).

## Share spreadsheet on node1

AccessPolicies = [{user2, write}, {user3, read}].
distributed_spreadsheet:share(my_spreadsheet,AccessPolicies).

## test reassign_owner

%%NewOwnerPid = spawn(fun() -> receive after infinity -> ok end end).

## Test scrittura / lettura

%% Integer value %% Atom valuen%% String (list) value %% Tuple value%% Map value
distributed_spreadsheet:set(my_spreadsheet, 1, 2, 3, 42).
distributed_spreadsheet:set(my_spreadsheet, 1, 2, 4, my_atom).
distributed_spreadsheet:set(my_spreadsheet, 1, 2, 5, "Hello").
distributed_spreadsheet:set(my_spreadsheet, 1, 3, 3, {ok, "Tuple"}).
distributed_spreadsheet:set(my_spreadsheet, 1, 3, 4, #{key => value}).

%% List of integers
distributed_spreadsheet:set(my_spreadsheet, 1, 2, 3, [1, 2, 3, 4]).

distributed_spreadsheet:get(my_spreadsheet,1,2,3).
distributed_spreadsheet:get(my_spreadsheet,1,1,2,2000).
distributed_spreadsheet:get(my_spreadsheet,2,2,2).

## Gestione persistenza

distributed_spreadsheet:to_csv("my_spreadsheet.csv",my_spreadsheet,5000).

distributed_spreadsheet:from_csv("my_spreadsheet.csv").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## AVVIO di un Cluster di  (3) Nodi con Directory di Mnesia separate
%lavora su tre directory separate
C:\Users\campus.uniurb.it\Erlang\node1_data
C:\Users\campus.uniurb.it\Erlang\node2_data
C:\Users\campus.uniurb.it\Erlang\node3_data 
% Nota q(). chiude la shell

erl -sname node1@DESKTOPQ2A2FL7 -setcookie mycookie
erl -sname node2@DESKTOPQ2A2FL7 -setcookie mycookie
erl -sname node3@DESKTOPQ2A2FL7 -setcookie mycookie

## Creazione dello Schema Mnesia

% nel nodo 1
c(mnesia_setup).

Nodes = ['node1@DESKTOPQ2A2FL7', 'node2@DESKTOPQ2A2FL7', 'node3@DESKTOPQ2A2FL7'].
mnesia_setup:setup_mnesia(Nodes).

% nel nodo 1 , 2 e 3
mnesia:start().
%nei vari nodi
net_adm:ping('node1@DESKTOPQ2A2FL7').
net_adm:ping('node2@DESKTOPQ2A2FL7').
net_adm:ping('node3@DESKTOPQ2A2FL7').

% Connetti manualmente i nodi
mnesia:change_config(extra_db_nodes, ['node2@DESKTOPQ2A2FL7', 'node3@DESKTOPQ2A2FL7']).



mnesia:change_table_copy_type(schema, node(), disc_copies).
mnesia:change_table_copy_type(spreadsheet_data, node(), disc_copies).
mnesia:change_table_copy_type(access_policies, node(), disc_copies).


% per ottenere l`elenco dei nodi connessi esplicitamente a fprmare  un cluster
mnesia:system_info(running_db_nodes).
