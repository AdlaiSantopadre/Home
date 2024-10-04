-module(distributed_spreadsheet). %ver 1.2
-behaviour(gen_server).

%% API functions exported
-export([new/1, new/4, reassign_owner/2,share/2,
         info/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Include the record definition
-include("spreadsheet.hrl").

%%% API FUNCTIONS %%%


% Funzione per creare un nuovo foglio di nome Name e dimensioni definite internamente
new(Name) ->
    N = 5,  % Default N (numero di righe)
    M = 5,  % Default M (numero di colonne)
    K = 2,   % Default K (numero di tab)
    new(Name, N, M, K).
    

%% Funzione per creare un nuovo foglio con dimensioni  passate per valori
new(Name, N, M, K) when is_integer(N), is_integer(M), is_integer(K), N > 0, M > 0, K > 0 ->    % guardie sui valori passati->
    %gen_server:call(Name, {new, Name, N, M, K}).
    case global:whereis_name(Name) of
        undefined ->
            LastModified= calendar:universal_time(),
            Owner =self(), %Il Pid del processo chiamante viene salvato in Owner 
            %Tabs = lists:map(fun(_) -> create_tab(N, M) end, lists:seq(1, K)),
        
            gen_server:start_link({global, Name}, ?MODULE, {Name, Owner, N, M, K, LastModified}, []);
                                  
        _ ->
            {error, already_exists}
    end;
new(_, _, _, _) ->
    {error, invalid_parameters}.
%% Reassign the owner of the spreadsheet
%% API function to reassign the owner of a spreadsheet
reassign_owner(SpreadsheetName, NewOwnerPid) when is_pid(NewOwnerPid) ->
    case global:whereis_name(SpreadsheetName) of
        undefined ->
            {error, spreadsheet_not_found};  % The spreadsheet process doesn't exist
        Pid when is_pid(Pid) ->
            io:format("Attempting to reassign owner to ~p~n", [NewOwnerPid]),
            %% Use gen_server:call/2 for synchronous communication
            try
                gen_server:call(Pid, {reassign_owner, NewOwnerPid}, 5000)  %% 5 seconds timeout
            catch
                _:_Error -> {error, timeout}  %% Handle timeout or other errors
            end
    end.


%% Stop the spreadsheet process
stop() ->
    gen_server:cast(self(), stop).



%% Get information about the spreadsheet
info(SpreadsheetName) ->
    %% Step 1: Check if the process is registered globally
    case global:whereis_name(SpreadsheetName) of
        undefined -> 
            io:format("Spreadsheet ~p not found globally.~n", [SpreadsheetName]),
            {error, spreadsheet_not_found};
        
        Pid when is_pid(Pid) ->
            %% Step 2: Check if the process is alive
            io:format("Spreadsheet ~p is registered globally with PID ~p~n", [SpreadsheetName, Pid]),
            
            %% nella versione distribuita il case 
            %% case erlang:is_process_alive(Pid) of
            %%    true -> genera errore perche is_process_alice(pid) vale solo localmente
                    %% Step 3: Log the system status of the process
                    %%Status = sys:get_status(Pid),
                    %%io:format("Spreadsheet process status: ~p~n", [Status]),

                    %% Step 4: Try to make the gen_server:call using PID directly
                    try
                        gen_server:call(Pid, get_info)
                    catch
                        Class:Reason ->
                            io:format("Error calling gen_server:call/2 with Pid: ~p, Reason: ~p, ~p~n", [Pid, Class, Reason]),
                            {error, {call_failed, Reason}}
                    end
                
                
    end.

        
%% API function to share access policies

share(SpreadsheetName, AccessPolicies) when is_list(AccessPolicies) ->
    case global:whereis_name(SpreadsheetName) of
        undefined ->
            {error, spreadsheet_not_found};  % If the process is not found

        Pid when is_pid(Pid) ->
            %% Validate the access policies
            case validate_access_policies(AccessPolicies) of
                ok ->
                    %% Use gen_server:call to send the request synchronously
                    try
                        gen_server:call(Pid, {share, AccessPolicies})
                    catch
                        _:Error ->
                            io:format("Error encountered: ~p~n", [Error]),
                            {error, internal_error}
                    end;

                {error, Reason} ->
                    {error, Reason}
            end
    end.
%% Validate the list of access policies
validate_access_policies([]) -> 
    ok;  % An empty list is valid

validate_access_policies([{Proc, AP} | Rest]) ->
    case validate_proc(Proc) of
        ok ->
            case validate_access_policy(AP) of
                ok -> validate_access_policies(Rest);  % Recursively validate the rest
                {error, invalid_access_policy} -> {error, {invalid_access_policy, AP}}
            end;
        {error, invalid_process} -> 
            {error, {invalid_process, Proc}}
    end;

validate_access_policies(_) -> 
    {error, malformed_access_policy}.  % Catch all for malformed policies
%% Validate a process identifier (either a PID or an atom)
validate_proc(Proc) when is_pid(Proc) ->
    case is_process_alive(Proc) of
        true -> ok;
        false -> {error, not_alive_process}
    end;

validate_proc(Proc) when is_atom(Proc) ->
    case global:whereis_name(Proc) of
        undefined -> {error, invalid_process};  % Invalid if not registered
        _ -> ok  % Valid if registered
    end;

validate_proc(_) -> 
    {error, invalid_process}.  % Invalid if neither a PID nor a registered process
%% Validate the access policy
validate_access_policy(read) -> ok;
validate_access_policy(write) -> ok;
validate_access_policy(_) -> {error, invalid_access_policy}.

%%% gen_server CALLBACKS %%%

%% Initialize the spreadsheet process

init({Name, Owner, N, M, K, LastModified}) ->
    %% Init with  parameters from new/4
    io:format("~p process .~n", [Name]),
     try
        Tabs = lists:map(fun(_) -> create_tab(N, M) end, lists:seq(1, K)),
        Policies = [{Owner, write}],  % Initial access policy
        State = #spreadsheet{name = Name, tabs = Tabs, owner = Owner, access_policies = Policies, last_modified = LastModified},
        {ok, State}
    catch
        Class:Reason ->
            io:format("Error in init/1: ~p ~p~n", [Class, Reason]),
            {stop, {init_failed, Reason}}
    end.
    
%% Handle synchronous calls(e.g., getting a cell, or getting spreadsheet info)
%% Handle the synchronous request to get the spreadsheet's info
handle_call(get_info, _From, State) when is_record(State, spreadsheet) ->
    #spreadsheet{name = Name, tabs = Tabs, owner = Owner, access_policies = Policies, last_modified = LastModified} = State,
    io:format("want info from ~p~n", [_From]),
    TotalTabs = length(Tabs),
    TotalCells = lists:sum([length(Tab) * length(lists:nth(1, Tab)) || Tab <- Tabs]),
    
 %% Split access policies into read and write permissions
    ReadPermissions = [Proc || {Proc, read} <- Policies],
    WritePermissions = [Proc || {Proc, write} <- Policies],

    %% Create the info result map
    Info = #{name => Name,
             owner => Owner,
             last_modified => LastModified,
             total_tabs => TotalTabs,
             total_cells => TotalCells,
             read_permissions => ReadPermissions,
             write_permissions => WritePermissions
            },
    {reply, {ok, Info}, State};
%% Handle the 'share' request in the gen_server
handle_call({share, NewPolicies}, {CallerPid, Alias}, State = #spreadsheet{owner = Owner, access_policies = CurrentPolicies}) ->
    %% Check if the calling process is the owner
    if 
        {CallerPid, Alias} =:= {Owner, Alias} ->
            %% Update the access policies
            UpdatedPolicies = update_policies(NewPolicies, CurrentPolicies),
            NewState = State#spreadsheet{access_policies = UpdatedPolicies},
            
            %% Log the updated policies (for debugging purposes)
            io:format("Updated access policies: ~p~n", [UpdatedPolicies]),
            
            %% Respond with success
            {reply, {ok, UpdatedPolicies}, NewState};
        
        true ->
            %% If not the owner, return an error
            {reply, {error, not_owner}, State}
    end;
%% In your gen_server module
handle_call({reassign_owner, NewOwnerPid}, {CallerPid, Alias}, State = #spreadsheet{owner = Owner}) ->
    %CallerPid = element(1, From),
    io:format("Received reassign_owner request from ~p to assign new owner ~p~n", [CallerPid, NewOwnerPid]),
    if
        %CallerPid =:= Owner ->
        {CallerPid, Alias} =:= {Owner, Alias} ->
            %% Update the owner in the state
            NewState = State#spreadsheet{owner = NewOwnerPid},
            io:format("Ownership reassigned from ~p to ~p~n", [Owner, NewOwnerPid]),
            {reply, {ok, owner_reassigned}, NewState};
        true ->
            %% Caller is not the owner; deny the request
            io:format("Unauthorized reassign_owner request from ~p~n", [CallerPid]),
            {reply, {error, unauthorized}, State}
    end.




%% Handle asynchronous casts (e.g., updating a cell, sharing, reassigning ownership)


handle_cast(stop, State) ->
    {stop, normal, State}.

%% Handle any other messages
handle_info(_Info, State) ->
    {noreply, State}.

%% Clean up when the process stops
terminate(_Reason, _State) ->
    ok.

%% Handle code change if the module is upgraded
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% HELPER FUNCTIONS %%%

%% Create a new tab with NxM cells
create_tab(N, M) ->
    lists:map(fun(_) -> lists:duplicate(M, undef) end, lists:seq(1, N)).

% funzione ausuliaria per permettere di moficare la lista di policy di accesso  
 %Remove duplicates where the process in ExistingPolicies is already represented in NewPolicies (either as a PID or registered name).
% Ensure that PIDs and registered names referring to the same process are handled correctly without introducing duplicates.

% è implementata la List comprehension [Expression || Pattern <- List, Condition]
update_policies(NewPolicies, ExistingPolicies) ->
    % Step 1: Filter ExistingPolicies to exclude entries that are already in NewPolicies
    FilteredExistingPolicies = [
        Policy || 
        {Proc, _} = Policy <- ExistingPolicies, 
        not (
            lists:keymember(Proc, 1, NewPolicies) orelse
            is_pid(Proc) andalso lists:any(fun({NewProc, _}) -> global:whereis_name(NewProc) == Proc end, NewPolicies) orelse
            not is_pid(Proc) andalso lists:any(fun({NewProc, _}) -> NewProc == global:whereis_name(Proc) end, NewPolicies)
        )
    ],

    % Step 2: Return the combined list of NewPolicies and FilteredExistingPolicies
    FilteredExistingPolicies ++ NewPolicies.
    %With this single list comprehension, you can effectively:

 

