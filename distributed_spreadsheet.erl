-module(distributed_spreadsheet). %ver 1.5
-behaviour(gen_server).

%% API functions exported
-export([new/1, new/4, get/4, get/5, set/5, set/6,  restore_owner/2, reassign_owner/2, share/2,
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

%% API function to restore the ownership of the spreadsheet if the current owner shell has crashed
restore_owner(SpreadsheetName, NewOwnerPid) when is_pid(NewOwnerPid) ->
    case global:whereis_name(SpreadsheetName) of
        undefined ->
            {error, spreadsheet_not_found};
        Pid when is_pid(Pid) ->
            %% Use gen_server:call to send a restore_owner request
            gen_server:call(Pid, {restore_owner, NewOwnerPid})
    end.


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



%% API function to get information about the spreadsheet
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

%% API function to get the value from a specific cell with default timeout (infinity)
get(SpreadsheetName, TabIndex, I, J) ->
    get(SpreadsheetName, TabIndex, I, J, infinity).

%% API function to get the value from a specific cell with a custom timeout
get(SpreadsheetName, TabIndex, I, J, Timeout) when is_integer(TabIndex), is_integer(I), is_integer(J)  ->
    case global:whereis_name(SpreadsheetName) of
        undefined ->
            {error, spreadsheet_not_found};
        Pid when is_pid(Pid) ->
            %% Make a gen_server:call with a timeout
            try
                gen_server:call(Pid, {get, TabIndex, I, J}, Timeout)
            catch
                _:_ -> {error, timeout}
            end
    end.

%% API function to set a value with default timeout (infinity)
set(SpreadsheetName, TabIndex, I, J, Value) ->
    set(SpreadsheetName, TabIndex, I, J, Value, infinity).

%% API function to set a value with a custom timeout
set(SpreadsheetName, TabIndex, I, J, Value, Timeout) 
    when is_integer(TabIndex), is_integer(I), is_integer(J) ->
    case global:whereis_name(SpreadsheetName) of
        undefined -> {error, spreadsheet_not_found};
        Pid when is_pid(Pid) ->
            %% Check that Value is a valid Erlang term, dynamically handle all types
            case validate_value(Value) of
                ok ->
                    try
                        gen_server:call(Pid, {set, TabIndex, I, J, Value}, Timeout)
                    catch
                         _:_ -> {error, timeout}
                    end;            
                {error, invalid_type} ->
                    {reply, {error, invalid_type}}
            end
        end.


            
    


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
    
%%%%%%%%%%% Handle synchronous calls

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
%% Handle restore_owner request
handle_call({restore_owner, NewOwnerPid}, From, State = #spreadsheet{owner = Owner}) ->
    CallerPid = element(1, From),
    io:format("Restore owner request from ~p for new owner ~p~n", [CallerPid, NewOwnerPid]),
    
    %% Check if the current owner is alive
    
        case is_process_alive(Owner) of
            
            true ->
            %% Case 1: The current owner's shell is still alive, so reject the restore request
                io:format("Owner's shell ~p is still alive, restore request denied~n", [Owner]),
                
                {reply, {error, owner_alive}, State};
            

            %% Case 2: The current owner's shell has crashed, allow the restore
            false ->
            io:format("Owner's shell ~p is not alive, restoring ownership to ~p~n", [Owner, NewOwnerPid]),
            NewState = State#spreadsheet{owner = NewOwnerPid},
            
        {reply, ok, NewState}
            
            end; 
    
%% handle call to reassign the owner(?!)
handle_call({reassign_owner, NewOwnerPid}, From, State = #spreadsheet{owner = Owner}) ->
    CallerPid = element(1, From),
    io:format("Received reassign_owner request from ~p to assign new owner ~p~n", [CallerPid, NewOwnerPid]),
    
    case  CallerPid of
        Owner ->
            %% Update the owner in the state
            NewState = State#spreadsheet{owner = NewOwnerPid},
            io:format("Ownership reassigned from ~p to ~p~n", [Owner, NewOwnerPid]),
            {reply, {ok, owner_reassigned}, NewState};
        _ ->
            
            io:format("Unauthorized reassign_owner request from ~p~n", [CallerPid]),
            {reply, {error, unauthorized}, State}
    end;

%% Handle the 'get' request in the gen_server
handle_call({get, TabIndex, I, J}, From, State = #spreadsheet{tabs = Tabs, access_policies = Policies}) ->
    CallerPid = element(1, From),
    io:format("Get request from ~p for Tab: ~p, Row: ~p, Col: ~p~n", [CallerPid, TabIndex, I, J]),

    %% Check if the calling process has read access
    case check_access(CallerPid, Policies, read) of
        ok ->
            %% Ensure the TabIndex is within bounds
            if
                TabIndex > length(Tabs) orelse TabIndex < 1 ->
                    io:format("Tab index ~p is out of bounds~n", [TabIndex]),
                    {reply, {error, out_of_bounds}, State};
                true ->
                    %% Retrieve the Tab (TabMatrix) at TabIndex
                    TabMatrix = lists:nth(TabIndex, Tabs),
                    %% Ensure Row index I is within bounds
                    if
                        I > length(TabMatrix) orelse I < 1 ->
                            io:format("Row index ~p is out of bounds in Tab ~p~n", [I, TabIndex]),
                            {reply, {error, out_of_bounds}, State};
                        true ->
                            %% Retrieve the Row and ensure Column index J is within bounds
                            Row = lists:nth(I, TabMatrix),
                            if
                                J > length(Row) orelse J < 1 ->
                                    io:format("Col index ~p is out of bounds in Row ~p, Tab ~p~n", [J, I, TabIndex]),
                                    {reply, {error, out_of_bounds}, State};
                                true ->
                                    %% Retrieve the value at position (I, J)
                                    Value = lists:nth(J, Row),
                                    io:format("Returning value: ~p for Tab: ~p, Row: ~p, Col: ~p~n", [Value, TabIndex, I, J]),
                                    {reply, {ok, Value}, State}
                            end
                    end
            end;
        {error, access_denied} ->
            io:format("Access denied for process ~p~n", [CallerPid]),
            {reply, {error, access_denied}, State}
    end;


handle_call({set, TabIndex, I, J, Value}, From, State = #spreadsheet{tabs = Tabs, access_policies = Policies}) ->
    CallerPid = element(1, From),
    io:format("Set request from ~p for Tab: ~p, Row: ~p, Col: ~p, Value: ~p~n", [CallerPid, TabIndex, I, J, Value]),

    %% Check if the calling process has write access
    case check_access(CallerPid, Policies, write) of
        ok ->
            %% Ensure the TabIndex is within bounds
            if
                TabIndex > length(Tabs) orelse TabIndex < 1 ->
                    io:format("Tab index ~p is out of bounds~n", [TabIndex]),
                    {reply, {error, out_of_bounds}, State};
                true ->
                    %% Retrieve the Tab (TabMatrix) at TabIndex
                    TabMatrix = lists:nth(TabIndex, Tabs),
                    %% Ensure Row index I is within bounds
                    if
                        I > length(TabMatrix) orelse I < 1 ->
                            io:format("Row index ~p is out of bounds in Tab ~p~n", [I, TabIndex]),
                            {reply, {error, out_of_bounds}, State};
                        true ->
                            %% Retrieve the Row and ensure Column index J is within bounds
                            Row = lists:nth(I, TabMatrix),
                            if
                                J > length(Row) orelse J < 1 ->
                                    io:format("Col index ~p is out of bounds in Row ~p, Tab ~p~n", [J, I, TabIndex]),
                                    {reply, {error, out_of_bounds}, State};
                                true ->
                                    %% Replace the value at position (I, J) and update state
                                    NewRow = replace_nth(J, Value, Row), %% Any valid Erlang term can be set
                                    NewTabMatrix = replace_nth(I, NewRow, TabMatrix),
                                    NewTabs = replace_nth(TabIndex, NewTabMatrix, Tabs),
                                    %% Update the last modified timestamp
                                    CurrentTime = calendar:universal_time(),
                                    NewState = State#spreadsheet{tabs = NewTabs, last_modified = CurrentTime},
                                    io:format("Value set successfully~n"),
                                    {reply, ok, NewState}
                            end
                    end
            end;
        {error, access_denied} ->
            io:format("Access denied for process ~p~n", [CallerPid]),
            {reply, {error, access_denied}, State}
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

%%% PRIVATE HELPER FUNCTIONS %%%

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

 
% Check if the calling process has the required access (read/write)
check_access(PidOrName, Policies, RequiredAccess) ->

    io:format("control if ~p  process has access to ~p in list ~p~n",[PidOrName,RequiredAccess,Policies]),

    % Resolve PidOrName to both PID and registered name if possible
    ResolvedPid = case is_pid(PidOrName) of
                     true -> PidOrName;
                     false -> Resolved = global:whereis_name(PidOrName),
                                io:format("PidOrName is a registered name resolving to: ~p~n", [Resolved]),
                               Resolved
                  end,
    % Get the registered name if PidOrName is a PID and registered
   RegisteredName = case is_pid(PidOrName) of
                         true -> find_registered_name(ResolvedPid);  % Get registered name for the PID
                         false -> PidOrName  % If it's already a name, keep it
                     end,
    io:format("ResolvedPid is ~p~n",[ResolvedPid]),io:format("registeredName is  ~p~n",[RegisteredName]),
    % Check if either the resolved PID or the registered name has the required access in the access policies
    case lists:keyfind(ResolvedPid, 1, Policies) of
        {ResolvedPid, Access} when Access == RequiredAccess ->io:format("tuple is ~p~n",[{ResolvedPid,Access}]),
            ok;
            _ ->               
                case lists:keyfind(RegisteredName, 1, Policies) of
                    {RegisteredName, Access} when Access == RequiredAccess -> io:format("tuple is ~p~n",[{RegisteredName,Access}]), ok;
                 _ -> {error, access_denied}
            end
    end.
%Funzione ausiliaria di check_access
%Trova il registered_name di un processo in base al suo PID
find_registered_name(Pid) ->
    lists:foldl(  %scorre tutta la lista registered() restituendo Name di Pid se esiste come Pid registrato
        fun(Name, Acc) ->
            case global:whereis_name(Name) of
                Pid when Pid =/= undefined -> Name;  % Return the name if it matches the PID
                _ -> Acc  % Otherwise, keep searching
            end
        end,
        undefined,
        registered()
    ).
%Funzione ausiliaria per rimpiazzare un elemento in una lista al valore di indice dato
replace_nth(Index, NewVal, List) ->
    {Left, [_|Right]} = lists:split(Index-1, List),
    Left ++ [NewVal] ++ Right.
%% Helper functions to Validate the list of access policies,valditate process identifier,
%% validate atom AP representing policy
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
%% Helper function to validate the type of Value
validate_value(Value) ->
    case is_basic_type(Value) of
        true -> ok;
        false -> {error, invalid_type}
    end.

%% Helper to check for basic types in Erlang
is_basic_type(Value) when is_integer(Value); 
                         is_float(Value);
                         is_atom(Value);
                         is_list(Value);
                         is_tuple(Value);
                         is_map(Value);
                         is_binary(Value) ->
    true;
is_basic_type(_) ->
    false.


