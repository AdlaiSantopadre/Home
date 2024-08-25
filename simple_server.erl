%mainly taken from
%https://riptutorial.com/erlang/example/24705/using-gen-server-behavior
%see also
%http://20bits.com/article/erlang-a-generic-server-tutorial
-module(simple_server).

-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3, handle_cast/2, handle_info/2, terminate/2, stop/0]).
%wrapping functions
-export([add/0, dec/0, addA/0, decA/0]).

start_link() ->
    %start_link/4 piglia 4 parametri%
    %1 -> tupla {local,name} o {global,name}
    %2 -> il modulo su cui verra' chiamata la init()
    %3 -> parametri di inizializzaione (passati alla init) e' il nostro loopData
    %4 -> opzioni da passare per settare dimensione heap, debug mode
    % spesso 4 e' lasciata come lista vuota []
    %?MODULE fa riferimento al modulo attuale, sostituito a tempo di compilazione
    Return = gen_server:start_link({local,?MODULE},?MODULE,0,[]),
    io:format("starting link ~p\n",[Return]),
    Return.

%viene chiamata dalla start_link%
%DEVE ritornare una coppia del tipo
%{ok,State}
%{ok,State,Timeout}
%{ok,State,hibernate}
%{stop,Reason}
%ignore
init(LoopData) ->
  io:format("Chiamata init con ~p\n",[LoopData]),
  {ok,LoopData}
.
%init() non deve far nulla, solo passare LoopData

% handle_call puo' ritornare un valore reply o noreply
% {reply,Risultato, NuovoStato}
% {noreply,NuovoStato}
handle_call(add, _From,State) ->
  NewState = State +1,
  {P,Ref} = _From,
  io:format("chiamata handle call add con stato ~p da ~p <--> ~p\n",[NewState,P, Ref]),
  {reply,{ok,NewState},NewState};
handle_call({add,N}, _From,State) ->
  NewState = State +N,
  {P,Ref} = _From,
  io:format("chiamata handle call add con stato ~p da ~p <--> ~p\n",[NewState,P, Ref]),
  {reply,{ok,NewState},NewState};  

handle_call(dec, _From,State) ->
  NewState = State -1,
  {P,_} = _From,
  io:format("chiamata handle call dec con stato ~p da ~p\n",[NewState,P]),
  %provare con 0 e vedere che succede
  {reply,{ok,NewState},NewState};

handle_call(stop,_,State) ->
  {stop, normal, State};

handle_call(_,_,State) ->
  io:format("ignoro handle_call\n"),
  {reply,{ok,State},State}.

%CAST e' sempre asincrona, non abbiamo il Pid di ritorno
handle_cast(add, State) ->
    Return = {noreply, State+1},
    io:format("chiamata handle_cast: ~p~n", [State+1]),
    Return;
handle_cast(dec, State) ->
      Return = {noreply, State-1},
      io:format("chiamata handle_cast: ~p~n", [State-1]),
      Return;
handle_cast({add,N},State) ->
        Return = {noreply, State+N},
      io:format("chiamata handle_cast: ~p~n", [State+N]),
      Return;

handle_cast(stop, State) ->
    {stop, normal, State};


handle_cast(_msg, State) ->
          io:format("ignoro handle_cast:~n"),
          {noreply,State}.



%handle info gestisce tutti i messaggi mandati al server
handle_info(_Info, State) ->
    io:format("--> Received message ~p .....\n",[_Info]),
      {noreply, State}.

terminate(_Reason, _State) ->
        ok.

add() -> gen_server:call(?MODULE,add).
%asynchronous call%
addA() -> gen_server:cast(?MODULE,add).

dec() -> gen_server:call(?MODULE,dec).
decA() -> gen_server:cast(?MODULE,dec).

stop() -> gen_server:cast(?MODULE,stop).

