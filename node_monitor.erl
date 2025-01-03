-module(node_monitor).
-behaviour(gen_server).

%% API
-export([start_link/1, add_node/1, remove_node/1]).
-export([init/1, handle_info/2, terminate/2, code_change/3,handle_cast/2]).

%% Avvia il gen_server per il monitoraggio dei nodi
start_link(Nodes) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Nodes, []).

%% Aggiungi un nodo da monitorare
add_node(Node) ->
    gen_server:cast(?MODULE, {add_node, Node}).

%% Rimuovi un nodo dal monitoraggio
remove_node(Node) ->
    gen_server:cast(?MODULE, {remove_node, Node}).

%% Inizializza il gen_server
init(Nodes) ->
    %% Inizia a monitorare i nodi
    lists:foreach(fun(Node) ->
                io:format("Monitoring node: ~p~n", [Node]),
    net_kernel:monitor_nodes(true) 
    end, Nodes),
    
    {ok, #{nodes => Nodes}}.

%% Gestione degli eventi di disconnessione e riconnessione
handle_info({nodedown, Node}, State) ->
    io:format("Node ~p went down~n", [Node]),
    %% Prova a riconnettere il nodo
    spawn(fun() -> try_reconnect(Node) end),
    {noreply, State};

handle_info({nodeup, Node}, State) ->
    io:format("Node ~p is up again~n", [Node]),
    {noreply, State}.

%% Gestione delle richieste di aggiunta e rimozione di nodi
handle_cast({add_node, Node}, State) ->
    io:format("Adding node to monitor: ~p~n", [Node]),
    net_kernel:monitor_nodes(true, [Node]),
    {noreply, maps:put(Node, true, State)};

handle_cast({remove_node, Node}, State) ->
    io:format("Removing node from monitor: ~p~n", [Node]),
    net_kernel:monitor_nodes(false, [Node]),
    {noreply, maps:remove(Node, State)}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
terminate(Reason, State) ->
    io:format("Terminating spreadsheet ~p with reason: ~p~n", [maps:get(name, State), Reason]),
    ok.
%% Tentativo di riconnessione
try_reconnect(Node) ->
    io:format("Attempting to reconnect to node: ~p~n", [Node]),
    case net_adm:ping(Node) of
        pong ->
            io:format("Successfully reconnected to node: ~p~n", [Node]);
        pang ->
            io:format("Failed to reconnect to node: ~p. Retrying in 5 seconds...~n", [Node]),
            timer:sleep(5000),
            try_reconnect(Node)
    end.
