-module(app_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({local, app_sup}, ?MODULE, []).

init([]) ->
    {ok, {
        {one_for_one, 10, 60},
        [
            % Supervisor per ogni foglio di calcolo
            {spreadsheet_supervisor,
             {spreadsheet_supervisor, start_link, []},
             transient,
             5000,
             supervisor,
             [spreadsheet_supervisor]},
             
            % Processo node_monitor supervisionato
            {node_monitor,
             {node_monitor, start_link, [[]]}, %% Lista iniziale vuota o passare nodi
             permanent, %% Può essere `transient` o `temporary` a seconda del caso
             5000,
             worker,
             [node_monitor]}
        ]
    }}.