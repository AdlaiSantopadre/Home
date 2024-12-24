-module(distributed_spreadsheet). % gen_server with Mnesia
-behaviour(gen_server).
%% API functions exported from assignement
-export([new/1, new/4, share/2]).%, start_spreadsheet/0
%% API
-export([start_link/1]).
%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Include the record definitions
-include("records.hrl").
%%%%%%%%%%%%%%%%%%%%%
%%% API FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%
%% Crea uno spreadsheet con valori di default
new(Name) ->
    N = 3,  % Default N (numero di righe)
    M = 4,  % Default M (numero di colonne)
    K = 2,   % Default K (numero di tab)
    new(Name, N, M, K).

%% Crea uno spreadsheet con parametri specifici
new(SpreadsheetName, N, M, K) ->
    %% Richiesta del pid del master di my_app 
    OwnerPid= application_controller:get_master(my_app),
    Args= {SpreadsheetName, N, M, K,OwnerPid}, 
    case spreadsheet_supervisor:start_spreadsheet(Args) of
    %% Invio richiesta ad app_sup per creare il supervisore specifico
        {ok, Pid} ->
            io:format("Spreadsheet ~p started successfully with PID ~p~n", [SpreadsheetName, Pid]),
            {ok, Pid};
        {error, Reason} ->
            io:format("Failed to start spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
            {error, Reason}
    end.
%% Permette l'accesso condiviso allo spreadsheet secondo una lista di politiche di accesso
share(SpreadsheetName, AccessPolicies) when is_list(AccessPolicies)->
case global:whereis_name(SpreadsheetName) of
        undefined ->
            {error, spreadsheet_not_found};  % If the process is not found
        Pid when is_pid(Pid) ->
            MasterPid= application_controller:get_master(my_app),
            gen_server:call({global, SpreadsheetName}, {share, SpreadsheetName, [{MasterPid, write}]})
end.




%% Avvia il gen_server
start_link(Args) ->
    io:format("Starting distributed_spreadsheet with args: ~p~n", [Args]),
    {SpreadsheetName, _, _, _, _} = Args,
    gen_server:start_link({global, SpreadsheetName}, ?MODULE, Args, []).


%% Inizializza il processo spreadsheet distribuito
init({SpreadsheetName, N, M, K, OwnerPid}) ->
    io:format("Initializing spreadsheet ~p with owner ~p~n", [SpreadsheetName, OwnerPid]),
%%
    case mnesia:transaction(fun() ->
        case mnesia:read({spreadsheet_owners, SpreadsheetName}) of
            [] -> %% New spreadsheet creation
                io:format("No existing data for spreadsheet ~p. Initializing records.~n", [SpreadsheetName]),
                Records = generate_records(SpreadsheetName, N, M, K),
                lists:foreach(fun(Record) -> mnesia:write(Record) end, Records),
                mnesia:write(#spreadsheet_owners{name = SpreadsheetName, owner = OwnerPid}),
                {new, ok};
            [#spreadsheet_owners{owner = OwnerPid}] -> %% Restart case
                io:format("Found existing owner ~p for spreadsheet ~p.~n", [OwnerPid, SpreadsheetName]),
                {existing, OwnerPid}
        end
    end) of
        {atomic, {new, ok}} ->
            {ok, #{name => SpreadsheetName, size => {N, M, K}, owner => OwnerPid}}; %% OK
        {atomic, {existing, ExistingOwner}} ->
            {ok, #{name => SpreadsheetName, size => {N, M, K}, owner => ExistingOwner}};
        {aborted, Reason} ->
            io:format("Failed to initialize spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
            {stop, Reason}
    end.


%%% gen_server CALLBACKS %%%

terminate(Reason, State) ->
    io:format("Terminating spreadsheet ~p with reason: ~p~n", [maps:get(name, State), Reason]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_call({share, SpreadsheetName, AccessPolicies}, {FromPid, _Alias}, State) ->
    io:format("Received share/2 for spreadsheet ~p from Pid ~p ~n", [SpreadsheetName, FromPid]),
    %% Verifica se il chiamante è il proprietario dello spreadsheet
    MasterPid= application_controller:get_master(my_app),
    case mnesia:transaction(fun() ->
        case mnesia:read({spreadsheet_owners,SpreadsheetName}) of
            [#spreadsheet_owners{owner = MasterPid}] -> ok;
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
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unsupported_operation}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%

%%genera tutti i record necessari per "rappresentare" lo spreadsheet.
%% usa list comprehensions per creare i record.
generate_records(Name, N, M, K) ->
    [#spreadsheet_data{name = Name, tab = Tab, row = Row, col = Col, value = undef}
     || Tab <- lists:seq(1, K),
        Row <- lists:seq(1, N),
        Col <- lists:seq(1, M)].

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


