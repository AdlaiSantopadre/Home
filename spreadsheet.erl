% modulo Erlang che implementa il foglio di calcolo come record rispettando le prime due specifiche 
% cioè la possibilita' di creare uno spreadsheet con K fogli di dimenione N x M prefissate, oppure di specificare i valori di N , M e K voluti:
-module(spreadsheet).
-export([new/1, new/4]).

-record(spreadsheet, {
    name,
    tabs = [],         % Lista di fogli (ogni foglio è una matrice NxM)
    owner = undefined  % Il proprietario del foglio
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
            % Registrazione del processo con il nome del foglio
            register(Name, self()),
            % Memorizzazione dello stato
            loop(Spreadsheet);
        _ ->
            {error, already_exists}
    end;
% restanti controlli sui parametri di new/4
new(_, _, _, _) ->
    {error, invalid_parameters}.


