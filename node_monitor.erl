-module(node_monitor).
-behaviour(gen_server).

%% API
-export([start_link/0]) .%%monitor_nodes/0
%% Callbacks
-export([init/1, handle_info/2, terminate/2, code_change/3]).

%% API Avvia il gen_server per il monitoraggio dei nodi
start_link() ->
    gen_server:start_link({local, node_monitor}, ?MODULE, [], []).

%% Inizializza il gen_server
%% Registra il processo con un nome globale basato sul nodo
init([]) ->
    % NodeName = node(),
    % GlobalName = list_to_atom("node" ++ atom_to_list(NodeName)),
    % case global:register_name(GlobalName, self()) of
    %%Connette i nodi
    net_adm:ping('Alice@DESKTOPQ2A2FL7'),
    net_adm:ping('Bob@DESKTOPQ2A2FL7'),
    net_adm:ping('Charlie@DESKTOPQ2A2FL7'),
    %registra e carica il codice compilato
    Nodes = nodes(),
    lists:foreach(fun(Node) ->
    %% Nome globale basato sul nodo
    GlobalName = list_to_atom("nodo" ++ atom_to_list(Node)),

    %% Esegue  l'RPC per registrare il nome globalmente sul nodo remoto
     %% Esegui l'RPC per creare un processo remoto e registrare il nome globalmente
    case rpc:call(Node, erlang, spawn, [fun() ->
        case global:register_name(GlobalName, self()) of
            yes -> io:format("Nome globale ~p registrato su ~p~n", [GlobalName, node()]);
            no -> io:format("Fallita la registrazione del nome globale ~p su ~p~n", [GlobalName, node()]);
            {error, Reason} -> io:format("Errore nella registrazione del nome globale ~p: ~p~n", [GlobalName, Reason])
        end
    end]) of
        Pid when is_pid(Pid) ->
            io:format("Processo remoto avviato con PID ~p su ~p~n", [Pid, Node]);
        {badrpc, Reason} ->
            io:format("Chiamata RPC fallita su nodo ~p: ~p~n", [Node, Reason])
    end
end, Nodes),
    %% Inizia il monitoraggio dei nodi
    net_kernel:monitor_nodes(true),
    io:format("Monitoraggio attivo su nodo di servizio: ~p~n", [node()]),
            {ok, #{}}.
    %     {error, Reason} ->
    %         io:format("Errore registrazione su ~p: ~p~n", [NodeName, Reason]),
    %         {stop, Reason}
    % end.

%% Gestione degli eventi di disconnessione e riconnessione
handle_info({nodedown, Node}, State) ->
    io:format("Nodo giù rilevato dal monitor: ~p~n", [Node]),
    %% Prova a riconnettere il nodo
    try_restart_node(Node),
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
  os:cmd("restart_node.bat").   
    % case net_adm:ping(Node) of
    %     pong ->
    %         io:format("Successfully reconnected to node: ~p~n", [Node]),
    %         Dir = ("C:/Users/campus.uniurb.it/Erlang/" ++ atom_to_list(Node) ++ "data"),
    %         rpc:call(Node, application, set_env, [mnesia, dir, Dir]),
    %         rpc:call(Node, mnesia, start, []);
    %     pang ->
    %         io:format("Failed to reconnect to node: ~p. Retrying in 5 seconds...~n", [Node]),
    %         timer:sleep(5000),
    %         try_reconnect(Node)
    % end.
