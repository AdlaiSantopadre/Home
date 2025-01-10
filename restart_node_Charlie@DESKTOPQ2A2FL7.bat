REM Nome del nodo
set NODE_NAME=Charlie
set COOKIE=mycookie

REM Directory di lavoro
set WORK_DIR=C:\Users\Campus.uniurb.it\Erlang
set MNESIA_DIR=%WORK_DIR%\%NODE_NAME%@DESKTOPQ2A2FL7_data

REM Avvia il nodo Erlang
REM erl -sname %NODE_NAME% -setcookie %COOKIE% -pa %WORK_DIR% -config %NODE_NAME% -eval "restart_node:init('%MNESIA_DIR%')."
start  erl -sname %NODE_NAME% -setcookie %COOKIE% -pa %WORK_DIR% -config %NODE_NAME% -eval "restart_node:init()."