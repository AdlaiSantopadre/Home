@echo off

REM Controlla se è  stato passato un parametro (nome del nodo)
if "%1"=="" (
    echo Errore: Specificare il nome del nodo come parametro.
    echo Utilizzo: restart_node.bat NOME_NODO
    exit /b 1
)

REM Nome del nodo
set NODE_NAME=%1
set COOKIE=mycookie

REM Directory di lavoro
set WORK_DIR=C:\Users\Campus.uniurb.it\Erlang
set MNESIA_DIR=%WORK_DIR%\%NODE_NAME%@DESKTOPQ2A2FL7_data
SET EBIN_DIR=C:\Users\campus.uniurb.it\Erlang\ebin
SET SRC_DIR=C:\Users\campus.uniurb.it\Erlang\src
REM Avvia il nodo Erlang
start powershell -NoExit -Command "[console]::ForegroundColor = 'White'; [console]::BackgroundColor = 'DarkBlue'; Clear-Host;& erl -sname %NODE_NAME%@DESKTOPQ2A2FL7 -setcookie %COOKIE% -pa %EBIN_DIR% -pa %SRC_DIR% -config %WORK_DIR%\config\%NODE_NAME% -eval "restart_node:init`(`)"
 