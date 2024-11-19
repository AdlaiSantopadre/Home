@echo off
setlocal

REM Imposta la directory di base
set BASE_DIR=C:\Users\campus.uniurb.it\Erlang

REM Crea le directory per i nodi
mkdir "%BASE_DIR%\node1"
mkdir "%BASE_DIR%\node2"
mkdir "%BASE_DIR%\node3"

REM Imposta il cookie per i nodi
set COOKIE=mycookie

REM Avvia il nodo 1 in una nuova finestra di PowerShell
start powershell -NoExit -Command "& erl -sname node1@DESKTOPQ2A2FL7 -setcookie %COOKIE% "

REM Avvia il nodo 2 in una nuova finestra di PowerShell
start powershell -NoExit -Command "& erl -sname node2@DESKTOPQ2A2FL7 -setcookie %COOKIE% "

REM Avvia il nodo 3 in una nuova finestra di PowerShell
start powershell -NoExit -Command "& erl -name node3@DESKTOPQ2A2FL7 -setcookie %COOKIE%" 

echo I nodi sono stati avviati.
Pause

