REM Imposta la directory di base
set BASE_DIR=C:\Users\campus.uniurb.it\Erlang

REM Crea le directory per i nodi 
mkdir "%BASE_DIR%\Alice_data"


mkdir "%BASE_DIR%\Bob_data"


mkdir "%BASE_DIR%\Charlie_data"


REM Imposta il cookie per i nodi
set COOKIE=mycookie

REM Avvia il nodo 1 in una nuova finestra di PowerShell
start powershell -NoExit -Command "& erl -sname Alice@DESKTOPQ2A2FL7 -setcookie %COOKIE% -pa %BASE_DIR% -config Alice"

REM Avvia il nodo 2 in una nuova finestra di PowerShell
start powershell -NoExit -Command "& erl -sname Bob@DESKTOPQ2A2FL7 -setcookie %COOKIE% -pa %BASE_DIR% -config Bob"

REM Avvia il nodo 3 in una nuova finestra di PowerShell
start powershell -NoExit -Command "& erl -sname Charlie@DESKTOPQ2A2FL7 -setcookie %COOKIE% -pa %BASE_DIR -config Charlie%"

echo I nodi sono stati avviati.

