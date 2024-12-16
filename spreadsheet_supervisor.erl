
-module(spreadsheet_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0, add_spreadsheet/5,register_owner/2,assign_new_owner/1]).

%% Supervisor callbacks
-export([init/1,handle_info/2,handle_call/3]).
%% Include the record definition
-include("records.hrl").
%% State management
-record(state, {owners = []}). % Lista di tuple {SpreadsheetName, OwnerPid}

%% Start the supervisor
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Supervisor initialization
init([]) ->
    {ok, {
        {one_for_one, 10, 60},  % One-for-one strategy
        [
            {spreadsheet_worker,
             {distributed_spreadsheet, start_link, []},
             transient, % Transient for dynamic workers
             5000,     % Restart timeout
             worker,
             [distributed_spreadsheet]} % Modules required
        ]
    }}.

%% API: avvia un nuovo processo gen_server per uno spreadsheet e registra il proprietario.
add_spreadsheet(SpreadsheetName, N, M, K, OwnerPid) ->
    case supervisor:start_child(?MODULE, [SpreadsheetName, N, M, K]) of
        {ok, GenServerPid} ->
            io:format("Spreadsheet ~p created with GenServer PID ~p~n", [SpreadsheetName, GenServerPid]),
            % Save the owner in the state and Mnesia
            case mnesia:transaction(fun() ->
                mnesia:write(#spreadsheet_owners{name = SpreadsheetName, owner = OwnerPid})
            end) of
                {atomic, ok} ->
                    io:format("Owner ~p registered for spreadsheet ~p~n", [OwnerPid, SpreadsheetName]),
                    register_owner(SpreadsheetName, OwnerPid),
                    {ok, GenServerPid};
                {aborted, Reason} ->
                    io:format("Failed to register owner for spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            io:format("Failed to create spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
            {error, Reason}
    end.

%% Register the owner and monitor it
register_owner(SpreadsheetName, OwnerPid) ->
    Ref = erlang:monitor(process, OwnerPid),
    case gen_server:call(?MODULE, {register_owner, SpreadsheetName, OwnerPid, Ref}) of
        ok -> ok;
        Error -> Error
    end.

%% Supervisor gen_server callbacks
handle_call({register_owner, SpreadsheetName, OwnerPid, Ref}, _From, State = #state{owners = Owners}) ->
    UpdatedOwners = [{SpreadsheetName, OwnerPid, Ref} | lists:keydelete(SpreadsheetName, 1, Owners)],
    {reply, ok, State#state{owners = UpdatedOwners}}.
% gestione in handle_info/2 per intercettare i messaggi 'DOWN' di OwnerPid
handle_info({'DOWN', Ref, process, OwnerPid, _Reason}, State = #state{owners = Owners}) ->
    % Find the spreadsheet associated with the OwnerPid
    case lists:keyfind(OwnerPid, 2, Owners) of
        {SpreadsheetName, OwnerPid, Ref} ->
            io:format("Owner ~p for spreadsheet ~p terminated.~n", [OwnerPid, SpreadsheetName]),
            case assign_new_owner(SpreadsheetName) of
                {ok, NewOwnerPid} ->
                    io:format("New owner ~p assigned to spreadsheet ~p~n", [NewOwnerPid, SpreadsheetName]),
                    NewOwners = [{SpreadsheetName, NewOwnerPid, erlang:monitor(process, NewOwnerPid)} | 
                                 lists:keydelete(SpreadsheetName, 1, Owners)],
                    {noreply, State#state{owners = NewOwners}};
                {error, Reason} ->
                    io:format("Failed to assign new owner for spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
                    {noreply, State}
            end;
        false ->
            io:format("No owner found for OwnerPid ~p.~n", [OwnerPid]),
            {noreply, State}
    end;

handle_info(Msg, State) ->
    io:format("Unhandled message: ~p~n", [Msg]),
    {noreply, State}.

%% Assign a new owner
assign_new_owner(SpreadsheetName) ->
    AvailableNodes = [Node || Node <- nodes(), mnesia:ping(Node) == pong],
    case AvailableNodes of
        [] ->
            io:format("No available nodes to assign new owner for ~p~n", [SpreadsheetName]),
            {error, no_available_nodes};
        [NewNode | _] ->
            % Use the first available node to assign a new owner
            NewOwnerPid = rpc:call(NewNode, erlang, self, []),
            case mnesia:transaction(fun() ->
                mnesia:write(#spreadsheet_owners{name = SpreadsheetName, owner = NewOwnerPid})
            end) of
                {atomic, ok} ->
                    {ok, NewOwnerPid};
                {aborted, Reason} ->
                    io:format("Failed to update owner in Mnesia: ~p~n", [Reason]),
                    {error, Reason}
            end
    end.
