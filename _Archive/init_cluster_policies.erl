-module(init_cluster_policies).
-export([init_cluster_policies/2]).
-include("records.hrl").

init_cluster_policies(Nodes, SpreadsheetName) ->
    %% Recupera il MasterPid per ogni nodo e registra globalmente il nome
    Policies = lists:map(fun(Node) ->
        %% Esegue il comando su ciascun nodo
        case rpc:call(Node, application_controller, get_master, [my_app]) of
            MasterPid when is_pid(MasterPid) ->
            
            %% Crea un atomo unico per il nodo
            GlobalName = list_to_atom("nodo" ++ atom_to_list(Node)),
            %% Registra globalmente il MasterPid
            case global:register_name(GlobalName, MasterPid) of
                yes ->
                    io:format("MasterPid ~p registered globally as ~p~n", [MasterPid, GlobalName]),
                    {GlobalName, read}; %tutti i nodo del cluster vengono inseriti con policy read
                no ->
                    io:format("Failed to register MasterPid ~p: ~n", [MasterPid]),
                    {error}
            end;
            undefined ->
                io:format("Failed to retrieve MasterPid from node ~p~n", [Node]),
                {error, undefined}
        end
    end, Nodes),
    io:format("Access policies  for spreadsheet ~p: ~p~n", [SpreadsheetName, Policies]),
    %% Inserisce le politiche di accesso nella tabella Mnesia
    case populate_access_policies(SpreadsheetName, Policies) of
        {atomic,ok} ->
            io:format("Access policies initialized for spreadsheet ~p: ~p~n", [SpreadsheetName, Policies]),
            {ok, Policies};
        {aborted, Reason} ->
            io:format("Failed to populate access policies: ~p~n", [Reason]),
            {error, Reason}
    end.
populate_access_policies(SpreadsheetName, Policies) ->
    mnesia:transaction(fun() ->
        %% Rimuovi le politiche esistenti per lo spreadsheet
        mnesia:delete({access_policies, SpreadsheetName}),
        %% Inserisci le nuove politiche
        lists:foreach(fun({ProcOrName, Access}) ->
            Record = #access_policies{name = SpreadsheetName, proc = ProcOrName, access = Access},
            io:format("Inserting access policy: ~p~n", [Record]),
            mnesia:write(Record)
        end, Policies),
        ok
    end).

