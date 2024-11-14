-module(test).
-export([new/1, new/4, loop/5]).

-record(spreadsheet, {
    name,
    tabs = [],
    owner = undefined,
    access_policies = []
}).

% Funzione per creare un nuovo foglio con dimensioni default (separato dal proprietario)
new(Name) ->
    N = 10,
    M = 10,
    K = 1,
    new(Name, N, M, K).

% Funzione per creare un nuovo foglio di calcolo con un processo separato
new(Name, N, M, K) when is_integer(N), is_integer(M), is_integer(K), N > 0, M > 0, K > 0 ->
    case whereis(Name) of
        undefined ->
            Owner = self(),  % Il proprietario è il processo che chiama new/4
            Pid = spawn(test, loop, [Name, Owner, N, M, K]),  % Crea il processo separato
            register(Name, Pid),  % Registra il nome del processo per facilitarne l'accesso
            {ok, Pid};  % Restituisce il Pid del processo creato
        _ ->
            {error, already_exists}
    end;
new(_, _, _, _) ->
    {error, invalid_parameters}.

% Funzione per creare un tab (una matrice NxM di celle)
create_tab(N, M) ->
    lists:map(fun(_) -> lists:duplicate(M, undef) end, lists:seq(1, N)).

% Loop che gestisce il processo del foglio di calcolo
loop(Name, Owner, N, M, K) ->
    Tabs = lists:map(fun(_) -> create_tab(N, M) end, lists:seq(1, K)),
    Spreadsheet = #spreadsheet{name = Name, tabs = Tabs, owner = Owner},
    loop(Spreadsheet).

loop(State = #spreadsheet{name = Name, tabs = Tabs, owner = Owner, access_policies = Policies}) ->
    receive
        

        stop ->
            ok;  % Termina il processo
        _Other ->
            loop(State)  % Continua a ricevere messaggi
    end.



