-module(distributed_spreadsheet). % gen_server with Mnesia
-behaviour(gen_server).
%% API functions exported from assignement
-export([new/1, new/4, share/2, get/4, get/5]).%, start_spreadsheet/0
%% API
-export([start_link/1,update_access_policies/3]).
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
            %%MasterPid= application_controller:get_master(my_app),
            gen_server:call({global, SpreadsheetName}, {share, SpreadsheetName, [AccessPolicies]})
end.
%% API function to get the value from a specific cell with default timeout (infinity)
get(SpreadsheetName, TabIndex, I, J) ->
    get(SpreadsheetName, TabIndex, I, J, infinity).

%% API function to get the value from a specific cell with a custom timeout
get(SpreadsheetName, TabIndex, I, J, Timeout) when is_integer(TabIndex), is_integer(I), is_integer(J)  ->
    case global:whereis_name(SpreadsheetName) of
        undefined ->
            {error, spreadsheet_not_founded};
        Pid when is_pid(Pid) ->
            MasterPid= application_controller:get_master(my_app),
            %% Make a gen_server:call with a timeout
            try
                io:format("Caller node with MasterPid: ~p~n", [MasterPid]),
                gen_server:call(Pid, {get, TabIndex, I, J, MasterPid}, Timeout)
            catch
                _:_ -> {error, timeout}
            end
    end.



%% Avvia il gen_server
start_link(Args) ->
    io:format("Starting distributed_spreadsheet with args: ~p~n", [Args]),
    {SpreadsheetName, _, _, _, _} = Args,
    gen_server:start_link({global, SpreadsheetName}, ?MODULE, Args, []).






%%% gen_server CALLBACKS %%%

terminate(Reason, State) ->
    io:format("Terminating spreadsheet ~p with reason: ~p~n", [maps:get(name, State), Reason]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inizializza il processo spreadsheet distribuito
init({SpreadsheetName, N, M, K, OwnerPid}) ->
    io:format("Initializing spreadsheet ~p with owner ~p~n", [SpreadsheetName, OwnerPid]),

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
%% Handle the 'share' request in the gen_server
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
             %% Step 1: Recupera le politiche esistenti
            ExistingPolicies = [
                {Policy#access_policies.proc, Policy#access_policies.access}
                || Policy <- mnesia:match_object(#access_policies{name = SpreadsheetName, proc = '_', access = '_'})
            ],
            io:format("Existing Policies: ~p~n", [ExistingPolicies]),
            case update_access_policies(SpreadsheetName, AccessPolicies,ExistingPolicies) of
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

%% Handle the 'get' request in the gen_server SpreadsheetName, N, M, K, OwnerPid
handle_call({get, TabIndex, I, J, MasterPid}, From, State) ->
    CallerPid = MasterPid,
    io:format("Get request from ~p for Tab: ~p, Row: ~p, Col: ~p~n", [CallerPid, TabIndex, I, J]),

     %% Check if the calling process has read access
     case check_access(CallerPid, read) of
         ok ->
            io:format("Returning value for Tab: ~p, Row: ~p, Col: ~p~n", [ TabIndex, I, J]);
    %          Ensure the TabIndex is within bounds
    %             if
    %             TabIndex > tab orelse TabIndex < 1 ->
    %                 io:format("Tab index ~p is out of bounds~n", [TabIndex]),
    %                 {reply, {error, out_of_bounds}, State};
    %             true ->
    %                 %% Retrieve the Tab (TabMatrix) at TabIndex
    %                 TabMatrix = lists:nth(TabIndex, Tabs),
    %                 %% Ensure Row index I is within bounds
    %                 if
    %                     I > length(TabMatrix) orelse I < 1 ->
    %                         io:format("Row index ~p is out of bounds in Tab ~p~n", [I, TabIndex]),
    %                         {reply, {error, out_of_bounds}, State};
    %                     true ->
    %                         %% Retrieve the Row and ensure Column index J is within bounds
    %                         Row = lists:nth(I, TabMatrix),
    %                         if
    %                             J > length(Row) orelse J < 1 ->
    %                                 io:format("Col index ~p is out of bounds in Row ~p, Tab ~p~n", [J, I, TabIndex]),
    %                                 {reply, {error, out_of_bounds}, State};
    %                             true ->
    %                                 %% Retrieve the value at position (I, J)
    %                                 Value = lists:nth(J, Row),
    %                                 io:format("Returning value: ~p for Tab: ~p, Row: ~p, Col: ~p~n", [Value, TabIndex, I, J]),
    %                                 {reply, {ok, Value}, State}
    %                         end
    %                end
    %        end;
        {error, access_denied} ->
            io:format("Access denied for process ~p~n", [CallerPid]),
            {reply, {error, access_denied}, State}
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

update_access_policies(SpreadsheetName, ExistingPolicies, NewPolicies) ->
    %% Preelabora NewPolicies per creare una mappa di PID e nomi globali
    ResolvedNewPolicies = resolve_policies(NewPolicies),

    %% Filtra le ExistingPolicies
    FilteredExistingPolicies = lists:filter(fun({Proc, _}) ->
        %% Risolvi Proc in ExistingPolicies
        case resolve_to_global_or_pid(Proc) of
            {ok, ResolvedProc} -> 
                %% Mantieni solo se ResolvedProc è presente in ResolvedNewPolicies
                maps:is_key(ResolvedProc, ResolvedNewPolicies);
            _ -> false
        end
    end, ExistingPolicies),

    %% Combina le politiche filtrate con NewPolicies
    UpdatedPolicies = FilteredExistingPolicies ++ NewPolicies,

    %% Aggiorna la tabella Mnesia
    mnesia:transaction(fun() ->
        %% Rimuovi le vecchie politiche
        mnesia:delete({access_policies, SpreadsheetName}),
        %% Inserisci le nuove politiche
        lists:foreach(fun({Proc, Access}) ->
            Record = #access_policies{name = SpreadsheetName, proc = Proc, access = Access},
            io:format("Inserting Record: ~p~n", [Record]),
            mnesia:write(Record)
        end, UpdatedPolicies),
        ok
    end).
%%   Verifica della politica di accesso in lettura  
check_access(CallerPid, read) ->
                             io:format("Verifica della politica di accesso in lettura per: ~p~n", [CallerPid]).





            
            