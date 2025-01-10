-module(node_monitor).
-behaviour(gen_server).

%% API
-export([start_link/0,monitor_nodes/0]).%%monitor_nodes/0

%% Callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3]).

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
%% Registra il processo con un nome globale basato sul processo node-_monitor in esecuzione nel nodo
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



code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
terminate(Reason, State) ->
    io:format("Terminating spreadsheet ~p with reason: ~p~n", [maps:get(name, State), Reason]),
    ok.
%% Tentativo di riconnessione,ripetuto ad intervallo dato da timer:sleep/1
try_restart_node(Node) ->
    io:format("Tentativo di riavvio per il nodo: ~p~n", [Node]),
    %% Converte il nome del nodo in un nome file batch
    BatFile = "restart_node_" ++ atom_to_list(Node) ++ ".bat",
    os:cmd("start cmd.exe /c " ++ BatFile),
    io:format("Tentativo di riavvio eseguito per il nodo: ~p con il file ~p~n", [Node, BatFile]).

try_reconnect(Node) ->
    io:format("Attempting to reconnect to node: ~p~n", [Node]),  

    case net_adm:ping(Node) of
        pong ->
            io:format("Successfully reconnected to node: ~p~n", [Node]),
            Dir = ("C:/Users/campus.uniurb.it/Erlang/" ++ atom_to_list(Node) ++ "data"),
            rpc:call(Node, application, set_env, [mnesia, dir, Dir]),
            rpc:call(Node, mnesia, start, []);
        pang ->
            io:format("Failed to reconnect to node: ~p. Retrying in 5 seconds...~n", [Node]),
            timer:sleep(5000),
            try_reconnect(Node)
    end.
