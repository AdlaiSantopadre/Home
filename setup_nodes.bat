REM Imposta la directory di base
set BASE_DIR=C:\Users\campus.uniurb.it\Erlang

REM Imposta il percorso del file sys.config


REM Crea le directory per i nodi e copia sys.config in ciascuna
mkdir "%BASE_DIR%\Alice_data"


mkdir "%BASE_DIR%\Bob_data"


mkdir "%BASE_DIR%\Charlie_data"


REM Imposta il cookie per i nodi
set COOKIE=mycookie

REM Avvia il nodo 1 in una nuova finestra di PowerShell
start powershell -NoExit -Command "& erl -sname Alice@DESKTOPQ2A2FL7 -setcookie %COOKIE% -pa %BASE_DIR% "

REM Avvia il nodo 2 in una nuova finestra di PowerShell
start powershell -NoExit -Command "& erl -sname Bob@DESKTOPQ2A2FL7 -setcookie %COOKIE% -pa %BASE_DIR%"

REM Avvia il nodo 3 in una nuova finestra di PowerShell
start powershell -NoExit -Command "& erl -sname Charlie@DESKTOPQ2A2FL7 -setcookie %COOKIE% -pa %BASE_DIR%"

echo I nodi sono stati avviati.

