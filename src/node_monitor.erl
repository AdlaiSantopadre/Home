-module(node_monitor).
-behaviour(gen_server).

%% API
-export([start_link/0,monitor_nodes/0]).%%monitor_nodes/0

%% Callbacks
-export([init/1, handle_info/2,handle_call/3,handle_cast/2, terminate/2, code_change/3]).

%% API Avvia il gen_server per il monitoraggio dei nodi
start_link() ->
    gen_server:start_link({local, node_monitor}, ?MODULE, [], []).
%% API per avviare il monitoraggio
monitor_nodes() ->
    case start_link() of
        {ok, _Pid} -> {ok, _Pid};
        {error, Reason} -> io:format("Errore monitoraggio nodo: ~p~n", [Reason])
    end.
%% Inizializza il gen_server
%% Registra il processo con un nome globale basato sul processo node_monitor in esecuzione nel nodo
init([]) ->
NodeName = node(),
    GlobalName = list_to_atom("node" ++ atom_to_list(NodeName)),
    case global:register_name(GlobalName, self()) of
        yes ->
            %% Inizia il monitoraggio dei nodi
            net_kernel:monitor_nodes(true),
            io:format("Nodo  registrato su ~p con nome globale: ~p~n", [self(), GlobalName]),
            {ok, #{}};
        {error, Reason} ->
            io:format("Errore registrazione su ~p: ~p~n", [self(), Reason]),
            {stop, Reason}
    end.

%% Gestione degli eventi di disconnessione e riconnessione
handle_info({nodedown, Node}, State) ->
    io:format("Nodo giù rilevato dal monitor: ~p~n", [Node]),
    %% Prova a riconnettere il nodo
    spawn(fun() -> try_restart_node(Node) end),
    {noreply, State};

handle_info({nodeup, Node}, State) ->
    io:format("Nodo di nuovo attivo: ~p~n", [Node]),
    
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unsupported_operation}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
terminate(Reason, State) ->
    io:format("Terminating spreadsheet ~p with reason: ~p~n", [maps:get(name, State), Reason]),
    ok.

%% Tentativo di riavvio
% try_restart_node(Node) ->
%     io:format("Tentativo di riavvio per il nodo: ~p da monitor ~p~n" , [Node,self() ]),
%     %% Converte il nome del nodo in un nome file batch
%     BatFile = "restart_node.bat" ++ atom_to_list(Node),
%     os:cmd("start cmd.exe /c " ++ BatFile),
%     io:format("Tentativo di riavvio eseguito per il nodo: ~p con il file ~p~n", [Node, BatFile]).
try_restart_node(Node) ->
    io:format("Tentativo di riavvio per il nodo: ~p da monitor ~p~n", [Node, self()]),

    %% Estrae il nome del nodo senza il dominio (@DESKTOPQ2A2FL7)
    [NodeName | _Domain] = string:split(atom_to_list(Node), "@", all),

    %% Converte il nome del nodo in un nome file batch
    BatFile = "restart_node.bat " ++ NodeName,
    %% Esegue il file batch usando os:cmd
    os:cmd("start cmd.exe /c " ++ BatFile),

    io:format("Tentativo di riavvio eseguito per il nodo: ~p con il file ~p~n", [Node, BatFile]).

