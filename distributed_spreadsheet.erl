-module(distributed_spreadsheet). %ver 1.7
-behaviour(gen_server).

%% API functions exported from assignement
-export([new/1, new/4,share/2, get/4, get/5, set/5, set/6,
        from_csv/1, to_csv/2, to_csv/3,info/1 ]).
%% API functions added
-export([restore_owner/2, reassign_owner/2,stop/1]).

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
%% API function to export spreadsheet data to CSV (with default timeout)
to_csv(Filename, SpreadsheetName) ->
    to_csv(Filename, SpreadsheetName, infinity).

%% API function to export spreadsheet data to CSV with a custom timeout
to_csv(Filename, SpreadsheetName, Timeout) when is_atom(SpreadsheetName) ->
    io:format("Starting to_csv with Filename: ~p and SpreadsheetName: ~p~n", [Filename, SpreadsheetName]),
    
    % Retrieve the PID associated with the registered name
    case global:whereis_name(SpreadsheetName) of
        undefined ->
            io:format("Error: Spreadsheet process not found for name: ~p~n", [SpreadsheetName]),
            {error, spreadsheet_not_found};  % If the process is not found
        Pid when is_pid(Pid) ->
            io:format("Found PID: ~p for SpreadsheetName: ~p~n", [Pid, SpreadsheetName]),
            % Use gen_server:call to request the spreadsheet state, with a custom timeout
            try
                gen_server:call(Pid, {export_to_csv, Filename}, Timeout)
            catch
                _:_Error ->
                    io:format("Error during CSV export operation~n"),
                    {error, timeout}
            end
    end.
%% API function to load spreadsheet data from CSV file
%% API function to load spreadsheet data from a CSV file
from_csv(Filename) ->
    %% Step 1: Open the CSV file and parse metadata (including SpreadsheetName)
    case file:open(Filename, [read]) of
        {ok, File} ->
            % Parse the spreadsheet metadata (including name) and tabs
            case parse_csv(File) of
                {ok, SpreadsheetState = #spreadsheet{name = SpreadsheetName}} ->
                    file:close(File),

                    % Step 2: Check if a process is registered under SpreadsheetName
                    case global:whereis_name(SpreadsheetName) of
                        undefined ->
                            %% No process registered -> Start a new gen_server process with the loaded state
                            io:format("Starting new spreadsheet process for ~p~n", [SpreadsheetName]),
                            gen_server:start_link({global, SpreadsheetName}, ?MODULE, SpreadsheetState, []),
                            {ok, SpreadsheetState};

                        Pid when is_pid(Pid) ->
                            %% Process already registered -> Update the existing gen_server with the new state
                            io:format("Updating existing spreadsheet process for ~p~n", [SpreadsheetName]),
                            gen_server:call(Pid, {load_from_csv, SpreadsheetState}),
                            {ok, SpreadsheetState}
                    end;
                {error, Reason} ->
                    file:close(File),
                    io:format("Error parsing CSV file: ~p~n", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Error opening file: ~p~n", [Reason]),
            {error, Reason}
    end.



%% Stop the spreadsheet process
stop(SpreadsheetName) ->
    gen_server:cast({global, SpreadsheetName}, stop).



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
init(Args) ->
    io:format("Init called with args: ~p~n", [Args]),
    case Args of
        % Case when loading from CSV with existing state
        #spreadsheet{name = Name, tabs = Tabs, owner = Owner, access_policies = Policies, last_modified = LastModified} ->
            io:format("Loading spreadsheet from CSV: ~p~n", [Name]),
            % No need to create new tabs, just use the state
            {ok, #spreadsheet{name = Name, tabs = Tabs, owner = Owner, access_policies = Policies, last_modified = LastModified}};
        
        % Case when initializing a new spreadsheet (from new/1 or new/4)
        {Name, Owner, N, M, K, LastModified} ->
            io:format("Starting new spreadsheet: ~p~n", [Name]),
            Tabs = lists:map(fun(_) -> create_tab(N, M) end, lists:seq(1, K)),  %% Create new tabs
            Policies = [{Owner, write}],
            State = #spreadsheet{name = Name, tabs = Tabs, owner = Owner, access_policies = Policies, last_modified = LastModified},
            {ok, State};
        
        % Catch-all clause for error handling
        _ ->
            io:format("Invalid init arguments: ~p~n", [Args]),
            {stop, {init_failed, function_clause}, Args}
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

%% Handle the 'export_to_csv' request in the gen_server
handle_call({export_to_csv, Filename}, _From, State = #spreadsheet{name = Name, tabs = Tabs, owner = Owner, access_policies = AccessPolicies, last_modified = LastModified}) ->
    io:format("Exporting spreadsheet ~p to CSV file ~p~n", [Name, Filename]),
    
    %% Open the file for writing
    case file:open(Filename, [write]) of
        {ok, File} ->
            io:format("Opened file: ~p~n", [Filename]),

            %% Write the spreadsheet metadata to the file
            io:format(File, "Spreadsheet Name: ~s~n", [atom_to_list(Name)]),
            io:format(File, "Owner: ~p~n", [Owner]),
            %% Convert access policies to a format suitable for CSV (PIDs to strings)
            write_access_policies_to_csv(File, AccessPolicies),
                
            
            write_last_modified_to_csv(File, LastModified),
            %% Write each tab (as rows) to the file in CSV format
            lists:foreach(fun(Tab) -> save_tab_to_csv(File, Tab) end, Tabs),
            
            %% Close the file
            file:close(File),
            io:format("Finished writing to CSV file: ~p~n", [Filename]),
            {reply, ok, State};
        
        {error, Reason} ->
            io:format("Error opening file ~p: ~p~n", [Filename, Reason]),
            {reply, {error, Reason}, State}
    end;

%% Handle the 'load_from_csv' request in the gen_server
handle_call({load_from_csv, NewSpreadsheetState}, _From, _CurrentState) ->
    %% Update the gen_server state with the new spreadsheet data
    io:format("Updating spreadsheet state from CSV~n"),
    {reply, ok, NewSpreadsheetState};

%% Handle the 'set' request in the gen_server
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
    io:format("Stopping the gen_server process~n"),
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
%% Check if the calling process has the required access (read/write)
check_access(PidOrName, Policies, RequiredAccess) ->
    io:format("Checking if ~p process has ~p access in policies: ~p~n", [PidOrName, RequiredAccess, Policies]),

    %% Resolve PidOrName to both PID and registered name if possible
    ResolvedPid = case is_pid(PidOrName) of
                     true -> PidOrName;
                     false -> global:whereis_name(PidOrName)
                  end,

    %% Check if the resolved PID or registered name has the 'write' access
    case lists:keyfind(ResolvedPid, 1, Policies) of
        {ResolvedPid, write} ->
            io:format("Access granted with superior write access for PID: ~p~n", [ResolvedPid]),
            ok;  %% If write access is found, return ok immediately
        _ ->
            %% Check for access by registered name (if available)
            case find_registered_name(ResolvedPid) of
                undefined -> check_read_access(PidOrName, Policies, RequiredAccess);
                RegisteredName ->
                    case lists:keyfind(RegisteredName, 1, Policies) of
                        {RegisteredName, write} ->
                            io:format("Access granted with superior write access for registered name: ~p~n", [RegisteredName]),
                            ok;  %% If write access is found by name, return ok immediately
                        _ -> check_read_access(RegisteredName, Policies, RequiredAccess)
                    end
            end
    end.

%% Helper function to check for read access
check_read_access(PidOrName, Policies, RequiredAccess) ->
    case lists:keyfind(PidOrName, 1, Policies) of
        {PidOrName, read} when RequiredAccess == read ->
            io:format("Read access granted for ~p~n", [PidOrName]),
            ok;
        _ -> {error, access_denied}
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
%% Save each tab (matrix) to CSV rows
save_tab_to_csv(File, Tab) ->
    lists:foreach(fun(Row) ->
        %% Format each row as a CSV line
        Line = lists:map(fun(Cell) -> format_cell(Cell) end, Row),
        %% Join cells with commas and write to file
        io:format(File, "~s~n", [string:join(Line, ",")])
    end, Tab).

%% Format individual cells for CSV output
format_cell(undef) -> "undef";
format_cell(Cell) ->
    case is_basic_type(Cell) of
        true -> io_lib:format("~p", [Cell]);
        false -> "unsupported"
    end.
%% Parse the spreadsheet data and metadata from the CSV file
%% Parse the spreadsheet data and metadata from the CSV file
%% Parse the spreadsheet data and metadata from the CSV file
%% Parse the spreadsheet data and metadata from the CSV file
parse_csv(File) ->
    % Read the first line (Spreadsheet Name)
    case io:get_line(File, '') of
        "Spreadsheet Name: " ++ SpreadsheetNameLine ->
            SpreadsheetName = string:strip(SpreadsheetNameLine, both, $\n),

            % Read the owner line
            case io:get_line(File, '') of
                "Owner: " ++ OwnerLine ->
                    Owner = string:strip(OwnerLine, both, $\n),

                    % Read the access policies line
                    case io:get_line(File, '') of
                        "Access Policies: " ++ AccessPoliciesString ->
                            case parse_term_from_string(AccessPoliciesString) of
                                {ok, AccessPoliciesStringTerm} ->
                                    case parse_access_policies(AccessPoliciesStringTerm) of
                                        {ok, AccessPolicies} ->

                                            % Read the last modified line
                                            case io:get_line(File, '') of
                                                "Last Modified: " ++ LastModifiedString ->
                                                    % Parse the last modified timestamp as a human-readable string
                                                    case parse_datetime(LastModifiedString) of
                                                        {error, Reason} ->
                                                            {error, Reason};
                                                        LastModifiedTuple ->
                                                            % Load the tabs (spreadsheet data)
                                                            Tabs = load_tabs_from_csv(File),
                                                            % Return the constructed spreadsheet state
                                                            {ok, #spreadsheet{
                                                                name = list_to_atom(SpreadsheetName),
                                                                tabs = Tabs,
                                                                owner = list_to_pid(Owner),
                                                                access_policies = AccessPolicies,
                                                                last_modified = LastModifiedTuple
                                                            }}
                                                    end;
                                                _ -> {error, invalid_last_modified}
                                            end;
                                        {error, Reason} -> {error, Reason}
                                    end;
                                {error, Reason} -> {error, Reason}
                            end;
                        _ -> {error, invalid_access_policies}
                    end;
                _ -> {error, invalid_owner}
            end;
        _ -> {error, invalid_spreadsheet_name}
    end.


%% Load the tabs (rows) from the CSV file
load_tabs_from_csv(File) ->
    case io:get_line(File, '') of
        eof -> [];  % End of file, no more rows
        Line ->
            % Split each line by commas and parse the cells
            Row = string:tokens(string:strip(Line, both, $\n), ","),
            [parse_row(Row) | load_tabs_from_csv(File)]
    end.
%% Parse a row (list of cells) into a list of Erlang terms
parse_row(Row) ->
    lists:map(fun parse_cell/1, Row).

%% Parse an individual cell, interpreting basic Erlang types
parse_cell("undef") -> undef;
parse_cell(Value) when is_number(Value) -> list_to_integer(Value);  % If it's a number, convert to integer
parse_cell(Value) ->
    case catch list_to_atom(Value) of
        {'EXIT', _} -> Value;  % If it can't be an atom, leave as string
        Atom -> Atom  % Convert to atom if possible
    end.
%% Helper function to parse a term from a string representation
parse_term_from_string(String) ->
    % Tokenize the string
    case erl_scan:string(String ++ ".") of  % Add a period to complete the expression
        {ok, Tokens, _} ->
            % Parse the tokens into an Erlang term
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> {ok, Term};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason, _} -> {error, Reason}
    end.
%% Convert access policies from string to valid terms (Strings to PIDs, atoms left unchanged)
parse_access_policies(AccessPoliciesString) ->
    AccessPolicies = lists:map(
        fun({Identifier, Access}) ->
            case catch list_to_pid(Identifier) of
                {'EXIT', _} ->  % If it's not a PID, assume it's a registered name (atom)
                    {Identifier, Access};
                Pid ->  % Convert valid PID strings back to PIDs
                    {Pid, Access}
            end
        end,
        AccessPoliciesString
    ),
    {ok, AccessPolicies}.
%% Convert access policies to a format suitable for CSV (PIDs to strings, atoms left unchanged)
write_access_policies_to_csv(File, AccessPolicies) ->
    AccessPoliciesString = lists:map(
        fun({Identifier, Access}) ->
            case is_pid(Identifier) of
                true -> {pid_to_list(Identifier), Access};  % Convert PIDs to strings
                false -> {Identifier, Access}  % Leave registered names (atoms) unchanged
            end
        end,
        AccessPolicies
    ),
    io:format(File, "Access Policies: ~p~n", [AccessPoliciesString]).

%% Convert {{Year, Month, Day}, {Hour, Minute, Second}} to a human-readable string
format_datetime({{Year, Month, Day}, {Hour, Minute, Second}}) ->
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                  [Year, Month, Day, Hour, Minute, Second]).

%% Write the last modified timestamp as a human-readable string
write_last_modified_to_csv(File, LastModified) ->
    LastModifiedString = lists:flatten(format_datetime(LastModified)),
    io:format(File, "Last Modified: ~s~n", [LastModifiedString]).



%% Parse a human-readable datetime string "YYYY-MM-DD HH:MM:SS" into {{Year, Month, Day}, {Hour, Minute, Second}}
%% Parse a human-readable datetime string "YYYY-MM-DD HH:MM:SS" into {{Year, Month, Day}, {Hour, Minute, Second}}
%% Parse a human-readable datetime string "YYYY-MM-DD HH:MM:SS" into {{Year, Month, Day}, {Hour, Minute, Second}}
parse_datetime(String) ->
    case string:tokens(String, " :-") of
        [YearS, MonthS, DayS, HourS, MinuteS, SecondS] ->
            {{list_to_integer(string:trim(YearS, both, [$\s, $\n, $\t])),
              list_to_integer(string:trim(MonthS, both, [$\s, $\n, $\t])),
              list_to_integer(string:trim(DayS, both, [$\s, $\n, $\t]))},
             {list_to_integer(string:trim(HourS, both, [$\s, $\n, $\t])),
              list_to_integer(string:trim(MinuteS, both, [$\s, $\n, $\t])),
              list_to_integer(string:trim(SecondS, both, [$\s, $\n, $\t]))}};
        _ ->
            {error, invalid_datetime_format}
    end.












