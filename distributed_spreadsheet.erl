-module(distributed_spreadsheet). %ver 1.0
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
            LastModified= erlang:system_time(),%calendar:universal_time(),
            Owner =self(), %Il Pid del processo chiamante viene salvato in Owner 
            %Tabs = lists:map(fun(_) -> create_tab(N, M) end, lists:seq(1, K)),
        
            gen_server:start_link({global, Name}, ?MODULE, {Name, Owner, N, M, K, LastModified}, []);
            % Restituisce il Pid del processo creato, per inviargli messaggi                       
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





%% Remove an access policy for a specific process (already implemente



%% Get information about the spreadsheet
info(SpreadsheetName) ->
    gen_server:call(SpreadsheetName, {info}).

%%% gen_server CALLBACKS %%%

%% Initialize the spreadsheet process

init({Name, Owner, N, M, K, LastModified}) ->
    %% Init with  parameters from new/4
    io:format("~p process .~n", [Name]),
    Tabs = lists:map(fun(_) -> create_tab(N, M) end, lists:seq(1, K)),
    Policies = [{Owner, write}],  % Initial access policy
    State = #spreadsheet{name = Name, tabs = Tabs, owner = Owner, access_policies = Policies, last_modified = LastModified},
    {ok, State}.

%% Handle synchronous calls(e.g., getting a cell, or getting spreadsheet info)



handle_call({get, TabIdx, Row, Col}, _From, State) ->
    Tabs = State#spreadsheet.tabs,
    CellValue = lists:nth(Col, lists:nth(Row, lists:nth(TabIdx, Tabs))),
    {reply, {ok, CellValue}, State};



handle_call({info}, _From, State) ->
    {reply, {ok, State}, State}.



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

