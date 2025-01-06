REM Imposta la directory di base
set BASE_DIR=C:\Users\campus.uniurb.it\Erlang

REM Imposta il cookie per i nodi
set COOKIE=mycookie

REM Array di nomi anglosassoni con iniziali alfabetiche
set NAMES=Alice Bob Charlie 
REM David Edward Frank Grace Helen Ian Jack

REM Crea le directory per i nodi ed avviali in loop
for %%N in (%NAMES%) do (
    REM Crea la directory per il nodo
    mkdir "%BASE_DIR%\%%N_data"

    REM Avvia il nodo in una nuova finestra di PowerShell
    start powershell -NoExit -Command "& erl -sname %%N@DESKTOPQ2A2FL7 -setcookie %COOKIE% -pa %BASE_DIR% -config %%N"
)

echo I nodi sono stati avviati.
