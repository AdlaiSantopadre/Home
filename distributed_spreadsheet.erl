-module(distributed_spreadsheet). %ver 1.2
-behaviour(gen_server).

%% API functions exported
-export([new/1, new/4, reassign_owner/2,
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
reassign_owner(SpreadsheetName, NewOwner) ->
    gen_server:cast(SpreadsheetName, {reassign_owner, NewOwner}).

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
            
            case erlang:is_process_alive(Pid) of
                true ->
                    %% Step 3: Log the system status of the process
                    Status = sys:get_status(Pid),
                    io:format("Spreadsheet process status: ~p~n", [Status]),

                    %% Step 4: Try to make the gen_server:call using PID directly
                    try
                        gen_server:call(Pid, get_info)
                    catch
                        Class:Reason ->
                            io:format("Error calling gen_server:call/2 with PID: ~p, Reason: ~p, ~p~n", [Pid, Class, Reason]),
                            {error, {call_failed, Reason}}
                    end;
                
                false ->
                    io:format("Process ~p is not alive.~n", [Pid]),
                    {error, process_not_alive}
            end;

        _Pid ->
            io:format("Invalid process registration for ~p: ~p~n", [SpreadsheetName, _Pid]),
            {error, invalid_process}
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

handle_call({get, TabIdx, Row, Col}, _From, State) ->
    Tabs = State#spreadsheet.tabs,
    CellValue = lists:nth(Col, lists:nth(Row, lists:nth(TabIdx, Tabs))),
    {reply, {ok, CellValue}, State}.




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

