-module(distributed_spreadsheet). % gen_server with Mnesia 
-behaviour(gen_server).
%% API functions exported from assignement
-export([new/1, new/4, share/2]). %  ,get/4, get/5, set/5, set/6,from_csv/1, to_csv/2, to_csv/3,info/1
        
%% gen_server callbacks
-export([init/1, handle_call/3,code_change/3]).%, handle_cast/2, handle_info/2, terminate/2, 

%% Include the record definition
-include("spreadsheet_data.hrl").
-include("spreadsheet_owners.hrl").
-include("access_policies.hrl").

%%% API FUNCTIONS %%%

new(Name) ->
    N = 3,  % Default N (numero di righe)
    M = 4,  % Default M (numero di colonne)
    K = 2,   % Default K (numero di tab)
    new(Name, N, M, K).

new(SpreadsheetName, N, M, K) when is_integer(N), is_integer(M), is_integer(K), N > 0, M > 0, K > 0 ->
    case global:whereis_name(SpreadsheetName) of
        undefined ->
            OwnerPid = self(),  % The shell that created the spreadsheet is the owner
            %% Spreadsheet doesn't exist, create it
            case gen_server:start_link({global, SpreadsheetName}, ?MODULE, {SpreadsheetName, N, M, K, OwnerPid}, []) of
            {ok, Pid} ->
                io:format("Spreadsheet ~p created with PID ~p~n", [SpreadsheetName, Pid]),
                {ok, Pid};
            Error ->
                    io:format("Failed to start spreadsheet process: ~p~n", [Error]),
                    Error
            end;
             _ ->
            {error, already_exists}
    end.
                     
               
%helper function
register_owner(SpreadsheetName, OwnerPid) ->
    case global:whereis_name({SpreadsheetName, owner}) of
        undefined ->
            %% Proceed with registration
            case global:register_name({SpreadsheetName, owner}, OwnerPid) of
                yes ->
                    io:format("Owner Pid ~p registered as {~p , owner} ~n", [OwnerPid, SpreadsheetName]),
                    mnesia:transaction(fun() ->
                                %% Save owner information
                                mnesia:write(#spreadsheet_owners{name = {SpreadsheetName,owner}, owner = OwnerPid})
                    end),         
                    %% Monitor the owner process
                    erlang:monitor(process, OwnerPid),
                    ok;
                no ->
                    io:format("Failed to register owner: name already registered for spreadsheet ~p~n", [SpreadsheetName]),
                    {error, name_already_registered};
                {error, Reason} ->
                    io:format("Failed to register ownership for spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
                    {error, Reason}
            end;
        ExistingPid ->
            io:format("Name already registered for spreadsheet ~p: ~p~n", [SpreadsheetName, ExistingPid]),
            {error, name_already_registered}
    end.

%%% gen_server CALLBACKS %%%

%% Initialize the spreadsheet process
init(Args) ->
    io:format("Init called with args: ~p~n", [Args]),
    case Args of
    % Case when initializing a new spreadsheet (e.g., from new/1 or new/4)
        {SpreadsheetName, N, M, K, OwnerPid} ->
            % Genera tutti i record per lo spreadsheet
            Records = generate_records(SpreadsheetName, N, M, K),
            io:format("Generati ~p record per lo spreadsheet ~p~n", [length(Records), SpreadsheetName]),
            % Inserisce tutti i record con una transazione Mnesia
            case mnesia:transaction(fun() ->
        
                lists:foreach(fun(Record) -> io:format("Inserisco record: ~p~n", [Record]),
                                             mnesia:write(Record) end, Records)
                %% Save owner information
                %%mnesia:write(#spreadsheet_owners{name = SpreadsheetName, owner = OwnerPid})
            end) of
                {atomic, ok} ->
                    %% Register owner globally
                    
                    case register_owner(SpreadsheetName, OwnerPid) of
                         ok ->
                            io:format("Spreadsheet ~p initialized successfully.~n", [SpreadsheetName]),
                            {ok, #{name => SpreadsheetName, size => {N, M, K}, owner => OwnerPid}};
                        {error, Reason} ->
                            io:format("Failed to register owner: ~p~n", [Reason]),
                            {stop, Reason}
                    end;
                {aborted, Reason} ->
                    io:format("Failed to initialize spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
                    {stop, Reason}
            end;
                
% Catch-all clause for invalid or unexpected arguments
        _ ->
            io:format("Invalid init arguments: ~p~n", [Args]),
            {stop, {init_failed, function_clause}, Args}  
end.
%helper function 
generate_records(Name, N, M, K) ->
    % Genera una lista di tutti i record #spreadsheet_data
    [#spreadsheet_data{name = Name, tab = Tab, row = Row, col = Col, value = undef}
     || Tab <- lists:seq(1, K),
        Row <- lists:seq(1, N),
        Col <- lists:seq(1, M)].    

%%% API FUNCTIONS %%%
share(SpreadsheetName, AccessPolicies) when is_list(AccessPolicies)->
case global:whereis_name(SpreadsheetName) of
        undefined ->
            {error, spreadsheet_not_found};  % If the process is not found

        Pid when is_pid(Pid) ->
            gen_server:call({global, SpreadsheetName}, {share, ottodicembre, [{self(), write}]})
end.

%%% gen_server CALLBACKS %%%
%% Handle the 'share' request in the gen_server

handle_call({share, SpreadsheetName, AccessPolicies}, {FromPid, _Alias}, State) ->
    io:format("Received share/2 for spreadsheet ~p from Pid ~p , Alias ~p  ~n", [SpreadsheetName, FromPid, _Alias]),
    %% Verifica se il chiamante è il proprietario dello spreadsheet
    case mnesia:transaction(fun() ->
        case mnesia:read(#spreadsheet_owners{name = SpreadsheetName}) of
            [#spreadsheet_owners{owner = FromPid}] -> ok;
            _ -> {error, unauthorized}
        end
    end) of
        {atomic, ok} ->
            %% Se autorizzato, aggiorna le politiche di accesso
            case update_access_policies(SpreadsheetName, AccessPolicies) of
                ok ->
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

update_access_policies(SpreadsheetName, AccessPolicies) ->
    mnesia:transaction(fun() ->
        %% Rimuovi tutte le politiche esistenti per lo spreadsheet
        mnesia:delete({access_policies, SpreadsheetName}),
        %% Inserisci le nuove politiche
        lists:foreach(fun({Proc, Access}) ->
            mnesia:write(#access_policies{
                name = SpreadsheetName,
                proc = Proc,
                access = Access
            })
        end, AccessPolicies),
        ok
    end).

%% Callback per aggiornamento a caldo
code_change(OldVsn, State, _Extra) ->
    
    %% Modifica dello stato, se necessario
    case OldVsn of
        undefined ->
         %   io:format("Upgrading code from version ~p with state ~p~n", [OldVsn, State])
            io:format("Upgrading code"),
            NewState = State; % Mantieni lo stato
        _ -> 
            %% Esempio: modifica dello stato durante l'upgrade
            NewState = maps:put(upgraded, true, State)
    end,
    {ok, NewState}.




