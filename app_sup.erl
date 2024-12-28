-module(app_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link({global, app_sup}, ?MODULE, []).

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
             [spreadsheet_supervisor]}
        ]
    }}.