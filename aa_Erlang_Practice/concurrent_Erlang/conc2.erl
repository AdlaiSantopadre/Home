-module(conc2).
-export([main/0, provaNB/0, miaSleep/1, miaFlush/0, simpleRegister/0, provaErrore/0, betterReg/0]).



provaNB() ->

    receive 
        {ping, Pid} -> io:format("sveglio !!! "), Pid!{pong}  
        %lista di pattern% 
    after
        %dopo 1 secondo mi sblocco
        10000 -> io:format("non ho ricevuto nulla ciao \n")    
    end
.


miaSleep(T) ->

    receive 
        %pattern vuoto che non matcha con niente%
        % _ ->  
    after T  -> io:format("che dormita \n")
    end
.


miaFlush() -> 

        receive
            X -> io:format(" Shell got ~p\n",[X]), miaFlush()
        after 0 -> ok
        end
.

main() -> 
    Attore = spawn(conc2, provaNB, []),
    %timer:sleep(4000),
    miaSleep(4000),    
    Attore!{ping, self()},
    receive
        {pong} -> io:format("ricevuto pong \n")
    %infinity è un atomo che sta per tempo indeterminato. A compile time tutte le receive
    %senza clausola after vengono compilate con after infinity    
    after
        infinity  -> ok   
    end
.


simpleRegister() ->
    %registro il risultato della spawn, che è un PID, col nome attore%
    %uso attore per interagire col nuovo processo creato
    register(attore, spawn( fun()-> receive {ciao, Pid} -> Pid!{ciao} end end)),
    attore!{ciao,self()}
 .   


provaErrore() -> 
    register(errore, spawn( fun() -> 1/0 end)),
    errore!{prova}
. 


betterReg() -> 
    %prima registro locale
    register(locale, self()),

    spawn(fun() ->      %prima registro il pid dell'attore
                        register(remoto, self()), 
                        %so che locale esiste perchè è stato creato alla riga 1    
                        locale!{remoto,ok},
                         receive 
                            {ciao} -> io:format("remoto ha ricevuto ciao \n")  
                         end 
                end ),
    receive
        {remoto, ok} -> remoto!{ciao}
    end
.