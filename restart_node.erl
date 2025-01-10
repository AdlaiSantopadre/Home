-module(restart_node).
-export([init/0]).%init(1)

%% Funzione principale per inizializzare il nodo
%init(MnesiaDir) ->
init() ->
    io:format("Inizializzazione del nodo ~p~n", [node()]),
    
    %% Riassegna il nome globale
    assign_global_name().
    
%     %% Configura e avvia Mnesia
%     setup_mnesia(MnesiaDir),
    
%     %% Avvia l'applicazione distribuita
%     start_application(),
    
%     io:format("Nodo ~p inizializzato correttamente~n", [node()]),
%     init:stop().

%% Riassegna il nome globale al nodo
assign_global_name() ->
    GlobalName = list_to_atom("nodo" ++ atom_to_list(node())),
   case  node_monitor:monitor_nodes() of
       
        {ok, _Pid} ->
            io:format("Nome globale ~p registrato correttamente~n", [GlobalName]);
        {error, Reason} ->
            io:format("Errore nella registrazione del nome globale: ~p~n", [Reason])
    end.

% %% Configura e avvia Mnesia
% setup_mnesia(MnesiaDir) ->
%     %% Configura la directory di Mnesia
%     application:set_env(mnesia, dir, MnesiaDir),
%     io:format("Configurazione della directory Mnesia: ~p~n", [MnesiaDir]),
    
%     %% Avvia Mnesia
%     case mnesia:start() of
%         ok ->
%             io:format("Mnesia avviata correttamente~n"),
%             load_tables();
%         {error, Reason} ->
%             io:format("Errore nell'avvio di Mnesia: ~p~n", [Reason])
%     end.

% %% Ricarica i dati delle tabelle
% load_tables() ->
%     Tables = mnesia:system_info(tables),
%     lists:foreach(fun(Table) ->
%         case mnesia:load_table(Table) of
%             {aborted, Reason} ->
%                 io:format("Errore nel caricamento della tabella ~p: ~p~n", [Table, Reason]);
%             _ ->
%                 io:format("Tabella ~p caricata correttamente~n", [Table])
%         end
%     end, Tables).

% %% Avvia l'applicazione OTP
% start_application() ->
%     case application:start(my_app) of
%         ok ->
%             io:format("Applicazione my_app avviata correttamente~n");
%         {error, Reason} ->
%             io:format("Errore nell'avvio dell'applicazione my_app: ~p~n", [Reason])
%     end.
