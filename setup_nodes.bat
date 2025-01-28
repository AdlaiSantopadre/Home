@echo off
SETLOCAL ENABLEDELAYEDEXPANSION
REM Imposta le directory di sorgente e di destinazione
SET SRC_DIR=C:\Users\campus.uniurb.it\Erlang\src
SET EBIN_DIR=C:\Users\campus.uniurb.it\Erlang\ebin
REM Esegue PowerShell per compilare tutti i file .erl
PowerShell -NoProfile -ExecutionPolicy Bypass -Command "Get-ChildItem -Path '%SRC_DIR%\*.erl' -Recurse | ForEach-Object { erlc -o '%EBIN_DIR%' $_.FullName }"
ENDLOCAL

REM Imposta la directory di base
set BASE_DIR=C:\Users\campus.uniurb.it\Erlang

REM Imposta il cookie per i nodi
set COOKIE=mycookie

REM Array di nomi anglosassoni con iniziali in ordine alfabetico
set NAMES=Alice Bob Charlie 
REM David Edward Frank Grace Helen Ian Jack

REM Crea le directory per i nodi Mnesia ed avviali in loop
for %%N in (%NAMES%) do (
    REM Rimuove la directory dati del nodo
if exist "%BASE_DIR%\%%N@DESKTOPQ2A2FL7_data" (
    rmdir /s /q "%BASE_DIR%\%%N@DESKTOPQ2A2FL7_data"
    echo Directory %BASE_DIR%\%%N@DESKTOPQ2A2FL7_data rimossa con successo.
) else (
    echo Directory %BASE_DIR%\%%N@DESKTOPQ2A2FL7_data non trovata.
)
    REM Crea la directory per il nodo
    mkdir "%BASE_DIR%\%%N@DESKTOPQ2A2FL7_data"

    REM Avvia il nodo in una nuova finestra di PowerShell
     start powershell -NoExit -Command "& erl -sname %%N@DESKTOPQ2A2FL7 -setcookie %COOKIE% -pa %BASE_DIR% %BASE_DIR%\ebin %BASE_DIR%\src -config %BASE_DIR%\config\%%N"
)
REM Avvia il nodo di monitor service 
SET EVAL_EXPR="spawn('Alice@DESKTOPQ2A2FL7', fun() -> group_leader(whereis(user), self()), demo_menu:start() end)."
echo %EVAL_EXPR%
start powershell -NoExit -Command "& erl -sname Monitor_service@DESKTOPQ2A2FL7 -setcookie %COOKIE% -pa %BASE_DIR% %BASE_DIR%\ebin %BASE_DIR%\src -eval "cluster_setup:start_cluster`(`)" -eval "observer:start`(`)" -eval "global:registered_names`(`)" -eval \"%EVAL_EXPR%\""
echo I nodi sono stati avviati.
