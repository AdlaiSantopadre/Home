% modulo Erlang che implementa il foglio di calcolo come record rispettando le prime due specifiche 
% cioè la possibilita' di creare uno spreadsheet con K fogli di dimenione N x M prefissate, oppure di specificare i valori di N , M e K voluti:
-module(spreadsheet).
-export([new/1, new/4, share/2]).

-record(spreadsheet, {
    name,
    tabs = [],              % Lista di fogli (ogni foglio è una matrice NxM)
    owner = undefined       % memorizza il proprietario del foglio
    access_policies = []    % Lista di tuple  {Proc, AP} con Proc uguale al Pid/reg_name per gestire le policy di accesso
                            % (read o write)
}).

% Funzione per creare un nuovo foglio di nome Name di dimensione 100 x 10
new(Name) ->
    N = 100,  % Default N (numero di righe)
    M = 10,  % Default M (numero di colonne)
    K = 1,   % Default K (numero di tab)
    new(Name, N, M, K).

% Funzione per creare un nuovo foglio con dimensioni passate per valori
new(Name, N, M, K) when is_integer(N), is_integer(M), is_integer(K), N > 0, M > 0, K > 0 ->    % guardie sui valori passati
    
    % Controllo se il nome del foglio esiste già!
    % La funzione whereis/1 in Erlang cerca il processo associato a un nome registrato. Se il nome è registrato, ritorna il pid (Process Identifier) del processo. 
    % Se il nome non è registrato, ritorna undefined.
    case whereis(Name) of
        undefined ->
            % Creazione del processo proprietario
            Owner = self(),
            % Creazione dei tab (K fogli, ciascuno una matrice NxM)
            Tabs = lists:map(fun(_) -> create_tab(N, M) end, lists:seq(1, K)),
            % Creazione del record spreadsheet
            Spreadsheet = #spreadsheet{name = Name, tabs = Tabs, owner = Owner},
            % REGISTRAZIONE DEL PROCESSO  CON IL NOME del foglio 
            register(Name, self()),
            % Memorizzazione dello stato
            loop(Spreadsheet);
        _ ->
            {error, already_exists}
    end;
% restanti controlli sui parametri di new/4
new(_, _, _, _) ->
    {error, invalid_parameters}.

% Funzione per creare una scheda come matrice NxM di celle
create_tab(N, M) ->
    lists:map(fun(_) -> lists:duplicate(M, undef) end, lists:seq(1, N)).
% Funzione che permette avvia l'aggiornamento delle policy di accesso con un messaggio spedito al Loop
share(SpreadsheetName, AccessPolicies) when is_list(AccessPolicies) ->
    case whereis(SpreadsheetName) of
        undefined ->
            {error, spreadsheet_not_found}; %verifica dell`esistenza del processo
        Pid ->
            Pid ! {share, self(), AccessPolicies},
            receive
                {share_result, Result} -> Result
            end
    end.
% Loop principale del processo, dove si gestiscono i messaggi
loop(State = #spreadsheet{owner = Owner, access_policies = Policies}) ->
    receive
     {share, From, AccessPolicies} ->
            if
                From =:= Owner ->
                    % Aggiornare le politiche di accesso
                    NewState = State#spreadsheet{access_policies = AccessPolicies},
                    From ! {share_result, true},
                    loop(NewState);
                true ->
                    % Se non è il proprietario, ritorna un errore
                    From ! {share_result, {error, not_owner}},
                    loop(State)
        % Gestire messaggi per operazioni sul foglio di calcolo
        stop -> 

            ok; % esce dal loop , ma attenzione non arresta Spreadsheet !!!
        _Other ->
            loop(State)
    end.



