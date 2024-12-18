-module(distributed_spreadsheet). % gen_server with Mnesia 
-behaviour(gen_server).

%% API functions exported from assignement
-export([new/1, new/4, share/2, start_spreadsheet/0]). %  ,get/4, get/5, set/5, set/6,from_csv/1, to_csv/2, to_csv/3,info/1
%% Helper functions
-export([update_access_policies/2]).  

%% gen_server callbacks
-export([init/1, handle_call/3,handle_info/2,terminate/2]).%,    

%% Include the record definition
-include("records.hrl").

%%% API FUNCTIONS %%%



%% Avvia il gen_server
%% Avvia il gen_server senza parametri iniziali
start_spreadsheet() ->
    gen_server:start_link(?MODULE, [], []).

%% Crea uno spreadsheet con valori di default
new(Name) ->
    N = 3,  % Default N (numero di righe)
    M = 4,  % Default M (numero di colonne)
    K = 2,   % Default K (numero di tab)
    new(Name, N, M, K).

%% Crea uno spreadsheet con parametri specifici
new(SpreadsheetName, N, M, K) when is_integer(N), is_integer(M), is_integer(K), N > 0, M > 0, K > 0 ->
%    ShellPid = self(), % Il Pid della shell chiamante è il proprietario iniziale
%    case spreadsheet_supervisor:add_spreadsheet(SpreadsheetName, N, M, K, ShellPid) of
%        {ok, GenServerPid} ->
%            io:format("Spreadsheet ~p created successfully with GenServer PID ~p~n", [SpreadsheetName, GenServerPid]),
%           {ok, #{name => SpreadsheetName, size => {N, M, K}, gen_server_pid => GenServerPid, owner_pid => ShellPid}};
    case supervisor:start_child(spreadsheet_supervisor, []) of
        {ok, Pid} ->
            %% Passa i parametri al worker usando una chiamata al gen_server
            gen_server:call(Pid, {init_spreadsheet, SpreadsheetName, N, M, K, self()});          
        {error, Reason} ->
            io:format("Failed to create spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
            {error, Reason}
    end.

%% Condivide lo spreadsheet (include  una lista di politiche)
share(SpreadsheetName, AccessPolicies) when is_list(AccessPolicies)->
case global:whereis_name(SpreadsheetName) of
        undefined ->
            {error, spreadsheet_not_found};  % If the process is not found

        Pid when is_pid(Pid) ->
            gen_server:call({global, SpreadsheetName}, {share, SpreadsheetName, [{self(), write}]})
end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inizializzazione senza parametri
init([]) ->
    io:format("Worker started, waiting for initialization.~n"),
    {ok, #{}}.

%% Gestisce l'inizializzazione dello spreadsheet
handle_call({init_spreadsheet, SpreadsheetName, N, M, K, OwnerPid}, _From, _State) ->
    io:format("Initializing spreadsheet ~p with owner ~p~n", [SpreadsheetName, OwnerPid]),
    
    %
    % Registra il proprietario globalmente
    ShellName = {SpreadsheetName, owner},
     case global:register_name(ShellName, OwnerPid) of
        yes ->
            Ref = erlang:monitor(process, OwnerPid),
            mnesia:transaction(fun() ->
                Records = generate_records(SpreadsheetName, N, M, K),
                lists:foreach(fun(Record) -> mnesia:write(Record) end, Records),
                case mnesia:write(#spreadsheet_owners{name = SpreadsheetName, owner = OwnerPid})of                             
                {atomic, ok} ->
                    io:format("Owner ~p registered for spreadsheet ~p~n", [OwnerPid, SpreadsheetName]),
                    erlang:monitor(process, OwnerPid), % Monitora direttamente l'OwnerPid
                    {ok, OwnerPid};
                {aborted, Reason} ->
                    io:format("Failed to register owner for spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
                    {error, Reason}
                 end
            end),
            {ok, #{name => SpreadsheetName, size => {N, M, K}, owner => OwnerPid, monitor_ref => Ref}};
        no ->
            {stop, already_registered};
        {error, Reason} ->
            {stop, Reason}
    end;
%    end) of
 %       {atomic, {ok, Ref}} ->
 %           {ok, #{name => SpreadsheetName, size => {N, M, K}, owner => OwnerPid,monitor_ref => Ref}};
  %      
 %       {aborted, Reason} ->
%            io:format("Failed to initialize spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
%            {stop, Reason}
 %   end.

%% Gestisce la condivisione dello spreadsheet

handle_call({share, SpreadsheetName, AccessPolicies}, {FromPid, _Alias}, State) ->
    io:format("Received share/2 for spreadsheet ~p from Pid ~p ~n", [SpreadsheetName, FromPid]),
    %% Verifica se il chiamante è il proprietario dello spreadsheet
    case mnesia:transaction(fun() ->
        case mnesia:read({spreadsheet_owners,SpreadsheetName}) of
            [#spreadsheet_owners{owner = FromPid}] -> ok;
            _ -> {error, unauthorized}
        end
    end) of
        {atomic, ok} ->
            %% Se autorizzato, aggiorna le politiche di accesso
            case update_access_policies(SpreadsheetName, AccessPolicies) of
                {atomic, ok} ->
                    io:format("Access policies for ~p updated successfully.~n", [SpreadsheetName]),
                    {reply, ok, State};
                {error, Reason} ->
                    io:format("Failed to update access policies for ~p: ~p~n", [SpreadsheetName, Reason]),
                    {reply, {error, Reason}, State}
            end;
        {atomic, {error, unauthorized}} ->
            io:format("Unauthorized access to update policies for ~p by ~p.~n", [SpreadsheetName, FromPid]),
            {reply, {error, unauthorized}, State};
        {aborted, Reason} ->
            io:format("Transaction failed while verifying ownership for ~p: ~p~n", [SpreadsheetName, Reason]),
            {reply, {error, Reason}, State}
    end.

%% Gestisce i messaggi di terminazione del proprietario
handle_info({'DOWN', _Ref, process, OwnerPid, Reason}, State = #{name := SpreadsheetName}) ->
    io:format("Owner process ~p terminated for spreadsheet ~p: ~p~n", [OwnerPid, SpreadsheetName, Reason]),
    ShellName = {SpreadsheetName, owner},
    % Rimuove il proprietario dalla tabella
    mnesia:transaction(fun() ->
        mnesia:delete({spreadsheet_owners, SpreadsheetName})
    end),
    % Attende la shell riavviata e la riassegna
    timer:sleep(1000), % Attendi un breve intervallo per il riavvio
    case global:whereis_name(ShellName) of
        undefined ->
            io:format("Shell for ~p not yet restarted.~n", [SpreadsheetName]),
            {stop, normal, State};
        NewOwnerPid ->
            io:format("Reassigning owner for ~p to new PID ~p~n", [SpreadsheetName, NewOwnerPid]),
            NewRef = erlang:monitor(process, NewOwnerPid),
            mnesia:transaction(fun() ->
                mnesia:write(#spreadsheet_owners{name = SpreadsheetName, owner = NewOwnerPid})
            end),
            {noreply, State#{owner => NewOwnerPid, monitor_ref => NewRef}}
    end;
handle_info(Msg, State) ->
    io:format("Unhandled message: ~p~n", [Msg]),
    {noreply, State}.

%% Terminazione del worker
terminate(Reason, State) ->
    io:format("Terminating spreadsheet ~p with reason: ~p~n", [maps:get(name, State), Reason]),
    ok.
%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%
update_access_policies(SpreadsheetName, NewPolicies) ->
    mnesia:transaction(fun() ->
        %% Step 1: Recupera le politiche esistenti
        ExistingPolicies = [
            {Policy#access_policies.proc, Policy#access_policies.access}
            || Policy <- mnesia:match_object(#access_policies{name = SpreadsheetName, proc = '_', access = '_'})
        ],
        io:format("Existing Policies: ~p~n", [ExistingPolicies]),

        %% Step 2: Filtra duplicati
        FilteredExistingPolicies = [
            Policy || 
            {Proc, _} = Policy <- ExistingPolicies, 
            not (
                lists:keymember(Proc, 1, NewPolicies) orelse
                is_pid(Proc) andalso lists:any(fun({NewProc, _}) -> global:whereis_name(NewProc) == Proc end, NewPolicies) orelse
                not is_pid(Proc) andalso lists:any(fun({NewProc, _}) -> NewProc == global:whereis_name(Proc) end, NewPolicies)
            )
        ],
        io:format("Filtered Existing Policies: ~p~n", [FilteredExistingPolicies]),

        %% Step 3: Combina le politiche
        UpdatedPolicies = FilteredExistingPolicies ++ NewPolicies,
        io:format("Updated Policies: ~p~n", [UpdatedPolicies]),

        
        %% Step 4: Rimuovi le vecchie politiche dalla tabella
        mnesia:delete({access_policies, SpreadsheetName}),

        %% Step 5: Inserisci nuove politiche
        lists:foreach(fun({Proc, Access}) ->
            Record = #access_policies{name = SpreadsheetName, proc = Proc, access = Access},
            io:format("Inserting Record: ~p~n", [Record]),
            mnesia:write(Record)
        end, UpdatedPolicies),

        ok
    end).




%helper functions %%

generate_records(Name, N, M, K) ->
    % Genera una lista di tutti i record #spreadsheet_data
    [#spreadsheet_data{name = Name, tab = Tab, row = Row, col = Col, value = undef}
     || Tab <- lists:seq(1, K),
        Row <- lists:seq(1, N),
        Col <- lists:seq(1, M)].    



