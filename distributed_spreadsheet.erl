-module(distributed_spreadsheet).
-behaviour(gen_server).

%% API
-export([start_link/1, new/4]).
%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% API per creare uno spreadsheet
new(SpreadsheetName, N, M, K) ->
    %% Invio richiesta ad app_sup per creare il supervisore specifico
    Args= {SpreadsheetName, N, M, K,self()}, 
    case spreadsheet_supervisor:start_spreadsheet(Args) of
        {ok, Pid} ->
            io:format("Spreadsheet ~p started successfully with PID ~p~n", [SpreadsheetName, Pid]),
            {ok, Pid};
        {error, Reason} ->
            io:format("Failed to start spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
            {error, Reason}
    end.

%% Avvia il gen_server
start_link(Args) ->
    io:format("Starting distributed_spreadsheet with args: ~p~n", [Args]),
    {SpreadsheetName, _, _, _, _} = Args,
    gen_server:start_link({global, SpreadsheetName}, ?MODULE, Args, []).

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
