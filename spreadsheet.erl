% modulo Erlang che implementa il foglio di calcolo come record rispettando le prime due specifiche 
% cioè la possibilita' di creare uno spreadsheet con K fogli di dimenione N x M prefissate, oppure di specificare i valori di N , M e K voluti:

-module(spreadsheet). %ver..3.0
-export([new/1, new/4, share/2, loop/5, remove_policy/2,to_csv/2, from_csv/1]).

-record(spreadsheet, {
    name,
    tabs = [],              % Lista di fogli (ogni foglio è una matrice NxM)
    owner = self(),       % memorizza il proprietario del foglio
    access_policies = []    % Lista di tuple  {Proc, AP} con Proc uguale al Pid/reg_name per gestire le policy di accesso
                            % (read o write)
}).

%% il foglio di calcolo verrà creato con un processo separato da quello del proprietario 
% Funzione per creare un nuovo foglio di nome Name di dimensione 100 x 10 e 1 tab
new(Name) ->
    N = 100,  % Default N (numero di righe)
    M = 10,  % Default M (numero di colonne)
    K = 1,   % Default K (numero di tab)
    new(Name, N, M, K).

% Funzione per creare un nuovo foglio con dimensioni passate per valori
new(Name, N, M, K) when is_integer(N), is_integer(M), is_integer(K), N > 0, M > 0, K > 0 ->    % guardie sui valori passati
    % Controllo se il nome del foglio esiste già!
    case whereis(Name) of
        undefined ->
            
            Owner =self(), %Il Pid del processo chiamante viene salvato in Owner e salvato nel campo omonimo del record #spreadsheet
            Spreadsheet = #spreadsheet{name = Name, tabs =[], owner = Owner, access_policies = []},
            Pid = spawn(spreadsheet, loop, [Name, Owner, N, M, K]),  % Avvio di un distinto processo  che esegue loop/5
            register(Name, Pid),  % REGISTRAZIONE DEL PROCESSO  CON IL NOME del foglio  per facilitarne l'accesso
            {ok, Pid};  % Restituisce il Pid del processo creato, per inviargli messaggi
            
            
        _ ->
            {error, already_exists}
    end;
% restanti controlli sui parametri di new/4
new(_, _, _, _) ->
    {error, invalid_parameters}.

% Funzione per creare una scheda come matrice NxM di celle
% lists:map(Function, List) applica la funzione specificata a ciascun elemento della lista fornita
% e restituisce una nuova lista con i risultati. 
%la lista su cui viene applicata la mappatura è quella generata da lists:seq(1, N)
% lists:duplicate(M, undef) crea una nuova lista lunga M di elementi il cui valore è undef per cui
%tale funzione chiamata su lists:seq(1,N) produce attraverso lists:map una matrice come lista di liste.
create_tab(N, M) ->
    lists:map(fun(_) -> lists:duplicate(M, undef) end, lists:seq(1, N)).

% Funzione che condivide il foglio e recepisce le policy di accesso con un messaggio spedito al Loop/1
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

% Funzione per rimuovere una policy di accesso per un processo specifico
remove_policy(SpreadsheetName, Proc) ->
    case whereis(SpreadsheetName) of
        undefined ->
            {error, spreadsheet_not_found};
        Pid ->
            Pid ! {remove_policy, self(), Proc},
            receive
                {remove_policy_result, Result} -> Result
            end
    end.
% Loop che gestisce il processo del foglio di calcolo
loop(Name, Owner, N, M, K) ->
    Tabs = lists:map(fun(_) -> create_tab(N, M) end, lists:seq(1, K)),
    Spreadsheet = #spreadsheet{name = Name, tabs = Tabs, owner = Owner,access_policies = []},
    loop(Spreadsheet).

% loop che gestisce lo State del foglio di calcolo e le operazioni sui dati
loop(State = #spreadsheet{name = Name, tabs = Tabs, owner = Owner, access_policies = Policies}) ->
    io:format("Spreadsheet process started. Waiting for messages.~n"),
    receive
        {get_spreadsheetPid, From} ->
            
                io:format("Received request for spreadsheet state from: ~p~n", [From]),
                io:format("State to be returned: ~p~n", [State]),  % Stampa il valore di `State`
                % Invia il record #spreadsheet{} indietro
                From ! {spreadsheet_state, State},
                io:format("Sent spreadsheet state to: ~p~n", [From]),
                loop(State);  % Continua ad ascoltare messaggi

        {share, From, NewPolicies} ->
            if
                From =:= Owner ->
                    % Aggiornare le politiche di accesso
                    NewState = State#spreadsheet{access_policies = NewPolicies},
                    From ! {share_result, ok},
                    loop(NewState);
                true ->
                    % Se non è il proprietario, ritorna un errore
                    From ! {share_result,{error, not_owner}},
                    loop(State)
            end;
        {remove_policy, From, Proc} ->
            if
                From =:= Owner ->
                    %lists:filter/2 è una funzione  prende una funzione (in questo caso un funtore) e una lista come argomenti.
                    %restituendo es una nuova lista composta solo dagli elementi che soddisfano 
                    %la condizione specificata nella funzione.Nel caso mantiene tutte le tuple {P,AP} della lista togliendo 
                    %solo quella che  corrisponde a P == Proc
                    NewPolicies = lists:filter(fun({P, _}) -> P =/= Proc end, Policies),
                    NewState = State#spreadsheet{access_policies = NewPolicies},
                    From ! {remove_policy_result, ok},
                    loop(NewState);
          
                true ->
                    From ! {error, not_owner},
                    loop(State)
            end;
        % Gestire messaggi per operazioni sul foglio di calcolo
        stop -> 

            ok; % esce dal loop , ma attenzione non arresta Spreadsheet !!!
        _Other ->
            loop(State)
    end.
% Funzione per salvare il foglio di calcolo in un file CSV, attraverso il registered name
% Funzione per salvare il foglio di calcolo in un file CSV, accetta il registered name
to_csv(Filename, SpreadsheetName) ->
    io:format("Starting to_csv with Filename: ~p and SpreadsheetName: ~p~n", [Filename, SpreadsheetName]),
    
    % Recupera il PID associato al nome registrato
    case whereis(SpreadsheetName) of
        undefined ->
            io:format("Error: Spreadsheet process not found for name: ~p~n", [SpreadsheetName]),
            {error, spreadsheet_not_found};  % Se il processo non è trovato
        Pid ->
            io:format("Found PID: ~p for SpreadsheetName: ~p~n", [Pid, SpreadsheetName]),

            % Invia un messaggio al processo per ottenere lo stato del foglio di calcolo
            Pid ! {get_spreadsheet, self()},
            receive
                {spreadsheet_state, #spreadsheet{name = Name, tabs = Tabs}} ->
                    io:format("Received spreadsheet state for Name: ~p~n", [Name]),
                    % Apri il file per la scrittura
                    case file:open(Filename, [write]) of
                        {ok, File} ->
                            io:format("Opened file: ~p~n", [Filename]),
                            % Scrivi il nome del foglio
                            io:format(File, "Spreadsheet Name: ~s~n", [atom_to_list(Name)]),
                            % Salva ogni tab come riga CSV
                            lists:foreach(fun(Tab) -> save_tab_to_csv(File, Tab) end, Tabs),
                            file:close(File),
                            io:format("Finished writing to CSV: ~p~n", [Filename]),
                            ok;
                        {error, Reason} ->
                            io:format("Error opening file: ~p, Reason: ~p~n", [Filename, Reason]),
                            {error, Reason}
                    end
            after 5000 ->
                io:format("Timeout while waiting for response from PID: ~p~n", [Pid]),
                {error, timeout}  % Timeout se il processo non risponde
            end
    end.

% Funzione per salvare ogni tab come riga CSV
save_tab_to_csv(File, Tab) ->
    lists:foreach(fun(Row) ->
        Line = lists:map(fun(Cell) -> format_cell(Cell) end, Row),
        io:format(File, "~s~n", [string:join(Line, ",")])
    end, Tab).

% Formattazione delle celle per CSV
format_cell(undef) -> "undef";
format_cell(Cell) -> io_lib:format("~p", [Cell]).


% Funzione per caricare il foglio di calcolo da un file CSV
from_csv(Filename) ->
    case file:open(Filename, [read]) of
        {ok, File} ->
            % Leggi il nome del foglio (prima riga)
            {ok, [SpreadsheetNameLine]} = io:get_line(File, ''),
            SpreadsheetName = string:strip(SpreadsheetNameLine, both, $\n),
            % Carica i tab da CSV
            Tabs = load_tabs_from_csv(File),
            file:close(File),
            % Costruisci il record Spreadsheet = #spreadsheet{} con i dati letti
            Spreadsheet = #spreadsheet{
                    name = list_to_atom(SpreadsheetName),  % Converte il nome in un atomo
                    tabs = Tabs,
                    owner = self(),  % Imposta il processo corrente come proprietario
                    access_policies = []  % Politiche di accesso vuote
                    },
            {ok, Spreadsheet};
        {error, Reason} ->
            {error, Reason}
    end.

% Funzione per caricare i tab dal file CSV
load_tabs_from_csv(File) ->
    case io:get_line(File, '') of
        eof -> [];  % Se abbiamo raggiunto la fine del file
        Line ->
            % Leggi ogni riga e trasformala in una lista di celle
            Row = string:tokens(string:strip(Line, both, $\n), ","),
            [parse_row(Row) | load_tabs_from_csv(File)]
    end.

% Funzione per interpretare una riga di celle
parse_row(Row) ->
    lists:map(fun parse_cell/1, Row).

% Funzione per interpretare il valore di una cella
parse_cell("undef") -> undef;  % Riconosci il valore 'undef'
parse_cell(Value) ->
    case catch list_to_atom(Value) of  % Cerca di convertire in atomo
        {'EXIT', _} -> Value;  % Se non è possibile, lascia il valore come stringa
        Atom -> Atom  % Converte in atomo se possibile
    end.




