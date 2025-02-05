SET EVAL_EXPR= "spawn('Alice@DESKTOPQ2A2FL7', fun() -> group_leader(whereis(user), self()), demo_menu:start() end)."
echo %EVAL_EXPR%
start powershell -NoExit -Command "[console]::ForegroundColor = 'White'; [console]::BackgroundColor = 'Blue'; Clear-Host;"