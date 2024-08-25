-module(lab).
-export([creaRing/3]).
%funzione forward

forward(M, Next) ->
        case M>0 of
          true ->
            receive 
            {messaggio}->
        end,
        forward(M,MioVicino)


.


%devo creare una rete ad anello di quattro attori, ogni processo crea il suo successivo
creaRing(N,M,PrimoPid) ->
  io:format("processo creato \n"),
    _MioVicino = 
        case N of 
        0 ->%devo riprendere il Pid del primo nodo
            PrimoPid!{all_created},
            PrimoPid;
            %altrimenti creo il prossimo nodo
        _ -> spawn(fun() -> 
                            creaRing(N-1, M, PrimoPid) , 
                            end)
    end,
    forward(M,MioVicino)
    .
 primo(M, Next) ->
 case M>0 of
   true -> Next!{messaggio}, 
            receive
            {messaggio}->
            primo(M-1,Next),
            end
            ;
     false-> receive
            {messaggio} -> io:format("mando stop\n",),
            Next!{stop}
            end
end  
         
                   
        
      
init(N,M) -> %  secondo è una rete di n-1 nodi
     Secondo = spawn(lab,creaRing,[N-1,M,self()]),
     receive
     {all_created} -> io:format("ricevuto all_created \n")
     end,
     primo(M,Secondo)
     .



     %esercizio che usa i registered name (il nome deve esistere prima di inviare msg ad esso)
     processo-i(I,N,M) ->
     MioID = "processo"++ integer_to_list