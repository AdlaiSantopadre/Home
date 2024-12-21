-module(distributed_spreadsheet).
-behaviour(gen_server).

%% API
-export([start_link/1, new/4]).
%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% API per creare uno spreadsheet
new(SpreadsheetName, N, M, K) ->
    %% Avvia il supervisore specifico
    case supervisor:start_child(app_sup, {SpreadsheetName, N, M, K, self()}) of
        {ok, _SupervisorPid} ->
            io:format("Spreadsheet ~p created successfully.~n", [SpreadsheetName]),
            {ok, SpreadsheetName};
        {error, Reason} ->
            io:format("Failed to create spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
            {error, Reason}
    end.

%% Avvia il gen_server
start_link({SpreadsheetName, N, M, K, OwnerPid}) ->
    gen_server:start_link({global, SpreadsheetName}, ?MODULE, {SpreadsheetName, N, M, K, OwnerPid}, []).

%% Callbacks
init({SpreadsheetName, N, M, K, OwnerPid}) ->
    io:format("Initializing spreadsheet ~p with owner ~p~n", [SpreadsheetName, OwnerPid]),
    {ok, #{name => SpreadsheetName, size => {N, M, K}, owner => OwnerPid}}.

terminate(Reason, State) ->
    io:format("Terminating spreadsheet ~p with reason: ~p~n", [maps:get(name, State), Reason]),
    ok.
%% Callbacks NON IMPLEMENTATE
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported_operation}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
