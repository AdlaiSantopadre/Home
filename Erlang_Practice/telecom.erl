-module(telecom).

init(N) -> provider:create_tables(),
provider:populate(N),
loop() %% per la persistenza
loop() -> receive % deve anche esso chiudersi con loop() per non terminaare
  {print}.

start()->
    spawn(telecom,init,[10]). %%così la start ritorna un pid , ma la funzione non va bene per una chiamata da remoto..
%% o mi scrivo la start remota
start_remota() ->
receive {stat,Pid} -> Server = spawn(telecom,init,[10]),
Pid!

%%ci creiamo anche un modulo (client)
