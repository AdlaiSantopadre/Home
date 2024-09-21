-module(spreadsheet). %ver..4.5
-export([new/1, new/4, share/2, starter/5, reassign_owner/2, remove_policy/2, to_csv/2, from_csv/1, get/4, get/5, set/5, set/6]).

-record(spreadsheet, {
    name,                % Nome del foglio di calcolo
    tabs = [],           % Dati (una lista di tab, una tab è una matrice)
    owner = undefined,   % Proprietario del foglio di calcolo (PID del processo creatore)
    access_policies = [] % Politiche di accesso (lista di tuple)
}).


% Funzione per creare un nuovo foglio di nome Name e dimensioni prescelte
new(Name) ->
    N = 5,  % Default N (numero di righe)
    M = 5,  % Default M (numero di colonne)
    K = 2,   % Default K (numero di tab)
    new(Name, N, M, K).

% Funzione per creare un nuovo foglio con dimensioni passate per valori
new(Name, N, M, K) when is_integer(N), is_integer(M), is_integer(K), N > 0, M > 0, K > 0 ->    % guardie sui valori passati
    % Controllo se il nome del foglio esiste già!
    case whereis(Name) of
        undefined ->
            
            Owner =self(), %Il Pid del processo chiamante viene salvato in Owner 
            %Tabs = lists:map(fun(_) -> create_tab(N, M) end, lists:seq(1, K)),
        
            Pid = spawn(spreadsheet, starter, [Name, Owner, N, M, K]),  % Avvio di un distinto processo  per lo spreadsheet
            register(Name, Pid),  % REGISTRAZIONE DEL PROCESSO  CON IL NOME del foglio  per facilitarne l'accesso
            {ok, Pid};  % Restituisce il Pid del processo creato, per inviargli messaggi
                       
        _ ->
            {error, already_exists}
    end;
% funzione per gestire chiamate scorrette di new/4
new(_, _, _, _) ->
    {error, invalid_parameters}.



% Funzione che avvia il processo del foglio di calcolo
starter(Name, Owner, N, M, K) ->
    io:format("~p  process started.~n",[Name]),
    Tabs = lists:map(fun(_) -> create_tab(N, M) end, lists:seq(1, K)),
    Spreadsheet = #spreadsheet{name = Name, tabs = Tabs, owner = Owner,access_policies = []},
    loop(Spreadsheet).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Funzione che permette di riassegnare  il proprietario  di spreadsheet se il processo chiamamante è il corrente proprietario
reassign_owner(SpreadsheetName, NewOwnerPid) when is_pid(NewOwnerPid) ->
    case whereis(SpreadsheetName) of
        undefined ->
            {error, spreadsheet_not_found};   % The spreadsheet process doesn't exist
        Pid ->
            io:format("try to reassign owner to ~p~n",[NewOwnerPid]),
            Pid ! {reassign_owner, self(), NewOwnerPid},
            receive
                {reassign_owner_result, Result} -> {ok,Result}
            after 5000 ->  % Timeout after 5000 milliseconds (5 seconds)
                    {error, timeout}    
            end
    end.
% La funzione get/4 spedisce un messaggio al processo spreadsheet richiedendo il valore di una specifica cella del foglio di calcolo
% senza limiti sul Timeout
% La funzione get/5 esegue lo stesso codice impostando un valore specifico per il Timeout

get(SpreadsheetName, TabIndex, I, J) ->
    get(SpreadsheetName, TabIndex, I, J, infinity). %infiniti riconduce get/4 a get/5

get(SpreadsheetName, TabIndex, I, J, Timeout) ->
    case whereis(SpreadsheetName) of
        undefined -> {error, spreadsheet_not_found};
        Pid ->
            % Check if the caller has read access
            %Prima di inviare la richiesta get al processo del foglio di calcolo, controlliamo se il processo %chiamante (self()) ha accesso in lettura. In caso contrario, restituiamo {error, access_denied}.
            %case check_access(self(), SpreadsheetName#spreadsheet.access_policies, read) of
            %    ok ->
                    Pid ! {get, self(), TabIndex, I, J},
                    receive
                        {get_result, Value} -> Value;
                        {error, access_denied} -> {error, access_denied}
                    after Timeout ->
                         {error, timeout}
                    end
                
            %end
    end.
%La funzione set/5 spedisce un messaggio al processo spreadsheet per aggiornare una cella specifica con un nuovo valore
% set/6 specifica ulteriormente un valore di Timeout desiderato
set(SpreadsheetName, TabIndex, I, J, Value) ->
    set(SpreadsheetName, TabIndex, I, J, Value, infinity).

set(SpreadsheetName, TabIndex, I, J, Value, Timeout) ->
    case whereis(SpreadsheetName) of
        undefined -> {error, spreadsheet_not_found};
        Pid ->
           % Send the get request to the spreadsheet process  
                %ERRATO
                %Prima di inviare la richiesta di set al processo del foglio di calcolo, controlliamo se il processo %chiamante (self()) ha accesso in scrittura. In caso contrario, restituiamo {error, access_denied}.
                %case check_access(self(), SpreadsheetName#spreadsheet.access_policies, write) of
                %ok ->

                    Pid ! {set, self(), TabIndex, I, J, Value},
                    receive
                        {set_result, ok} -> ok;
                        {error, access_denied} -> {error, access_denied}
                    after Timeout ->
                         {error, timeout}
                    end
                %end
    end.


%Funzione ausiliaria per rimpiazzare un elemento in una lista al valore di indice dato
replace_nth(Index, NewVal, List) ->
    {Left, [_|Right]} = lists:split(Index-1, List),
    Left ++ [NewVal] ++ Right.

% Check if the calling process has the required access (read/write)
check_access(PidOrName, Policies, RequiredAccess) ->

    io:format("control if ~p  process has access to ~p in list ~p~n",[PidOrName],[RequiredAccess],[Policies]),

    % Resolve the PID if the process is a registered name
    ResolvedPid = case is_pid(PidOrName) of
                      true -> PidOrName;
                      false -> whereis(PidOrName)
                  end,

    % Check if the resolved PID has the required access in the access policies
    case lists:keyfind(ResolvedPid, 1, Policies) of
        {ResolvedPid, Access} when Access == RequiredAccess -> ok;
        _ -> {error, access_denied}
    end.
% Funzione per condivide il foglio di lavoro definendo  le policy di accesso (una lista di tuple della forma {Proc, AP) dove Proc è un Pid o il suo nome registrato,
%e AP la sua policy di accesso read|write)

share(SpreadsheetName, AccessPolicies) when is_list(AccessPolicies) ->
    try

        case whereis(SpreadsheetName) of
            undefined ->
                {error, spreadsheet_not_found}; %verifica dell`esistenza del processo
            Pid ->
                %convalida le policies
                case validate_access_policies(AccessPolicies) of
                 ok ->
                        Pid ! {share, self(), AccessPolicies}, % Invia il nuovo messaggio per aggiornare le politiche
                        io:format("Sent request for share AccessPolicies ~p~n", [AccessPolicies]),
                        receive
                        {share_result, Result} -> Result
                    end;

                    {error,Reason} ->
                        {error, Reason}
                    end
        end
catch
     _:Error -> io:format("Error encountered: ~p~n", [Error])
end.
%funzioni  ausiliarie per convalidare le policies di accesso

    %validazione tuple politiche di accesso
validate_access_policies([]) -> ok;  % If the list is empty, it's valid
validate_access_policies([{Proc, AP} | Rest]) ->
    case validate_proc(Proc) of
        ok ->
            case validate_access_policy(AP) of
                ok -> 
                    validate_access_policies(Rest);  %Valida ricorsivamente Rest
                {error, invalid_access_policy} -> 
                    {error, {invalid_access_policy, AP}}  % Return error for invalid AP
            end;
        {error, invalid_process} -> 
            {error, {invalid_process, Proc}}  % Return error for invalid Proc
    end;
validate_access_policies(_) ->
    {error, malformed_access_policy}.
    % Validazione di un processo (Proc)
validate_proc(Proc) when is_pid(Proc) ->
    case is_process_alive(Proc) of
         true -> ok;
        _proc ->  {error, not_alive_process}  % Invalid 
         
    end;
validate_proc(Proc) when is_atom(Proc) ->
    case whereis(Proc) of
        undefined -> {error, invalid_process};  % Invalid if not a registered process
        _ -> ok  % Valid if it's a registered process
    end;
validate_proc(_) ->
    {error, invalid_process}.  % Invalid if it's neither a PID nor a registered process


    % Validazione della policy (AP)
validate_access_policy(read) -> ok;
validate_access_policy(write) -> ok;
validate_access_policy(_) -> {error, invalid_access_policy}.



% funzione ausuliaria per permettere di moficare la lista di policy di accesso  
 %Remove duplicates where the process in ExistingPolicies is already represented in NewPolicies (either as a PID or registered name).
% Ensure that PIDs and registered names referring to the same process are handled correctly without introducing duplicates.

% è implementata la List comprehension [Expression || Pattern <- List, Condition]
update_policies(NewPolicies, ExistingPolicies) ->
    % Step 1: Filter ExistingPolicies to exclude entries that are already in NewPolicies
    FilteredExistingPolicies = [
        Policy || 
        {Proc, _} = Policy <- ExistingPolicies, 
        not (
            lists:keymember(Proc, 1, NewPolicies) orelse
            is_pid(Proc) andalso lists:any(fun({NewProc, _}) -> whereis(NewProc) == Proc end, NewPolicies) orelse
            not is_pid(Proc) andalso lists:any(fun({NewProc, _}) -> NewProc == whereis(Proc) end, NewPolicies)
        )
    ],

    % Step 2: Return the combined list of NewPolicies and FilteredExistingPolicies
    FilteredExistingPolicies ++ NewPolicies.
    %With this single list comprehension, you can effectively:

 
% Funzione per rimuovere una policy di accesso (per un processo specifico)
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



% Funzione per creare una scheda come matrice NxM di celle
create_tab(N, M) ->
    lists:map(fun(_) -> lists:duplicate(M, undef) end, lists:seq(1, N)).
%%%%%%%%%%LOOP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% loop che gestisce lo State del foglio di calcolo e le operazioni sui dati
loop(State = #spreadsheet{name = Name, tabs = Tabs, owner = Owner, access_policies = Policies}) ->
    io:format("Spreadsheet State waiting for messages.~n"),
    
    receive
         % Handle ownership reassignment
        {reassign_owner, From, NewOwner} ->
            io:format("want reassign from ~p to ~p~n", [#spreadsheet.owner, NewOwner]),
            if
                From =:= NewOwner ->  % Only the restarted shell can reassign ownership

                    NewState = State#spreadsheet{owner = NewOwner},
                    io:format("Ownership reassigned from ~p to ~p~n", [Owner, NewOwner]),
                    From ! {reassign_owner_result, ok},
                    loop(NewState);
                true ->
                    From ! {reassign_owner_result, {error, unauthorized}},
                    loop(State)
            end;
 
% Handle get request 
        %la Tab(la matrice) restituita da lists:nth(Tab, Tabs) è assegnata a TabMatrix
        {get, From, Tab, I, J} ->
            % Check if the requesting process has read access
            case check_access(From, Policies, read) of
                ok ->      
                    io:format("Received get request for Tab: ~p, Row: ~p, Col: ~p~n", [Tab, I, J]),
                        if
                            Tab > length(Tabs) orelse Tab < 1 ->
                                io:format("Tab index ~p is out of bounds~n", [Tab]),
                                From ! {cell_value, undef};
                        true ->
                            TabMatrix = lists:nth(Tab, Tabs),
                            if
                                I > length(TabMatrix) orelse I < 1 ->
                                    io:format("Row index ~p is out of bounds in Tab ~p~n", [I, Tab]),
                                    From ! {cell_value, undef}; %If the indices are out of bounds, we return undef instead of trying to access an invalid position.
                                true ->
                                Row = lists:nth(I, TabMatrix),
                                    if
                                        J > length(Row) orelse J < 1 ->
                                            io:format("Col index ~p is out of bounds in Row ~p, Tab ~p~n", [J, I, Tab]),
                                            From ! {cell_value, undef};
                                    true ->
                                        Value = lists:nth(J, Row),
                                    
                                        From ! {cell_value, Value}
                                    end
                            end
                        end,
                        loop(State);
                {error, access_denied} ->
                    From ! {error, access_denied},
                    loop(State)
            end;
% Handle set request
        {set, From, Tab, I, J, Val} ->
        % Check if the requesting process has write access
            case check_access(From, Policies, write) of
                ok ->

                    io:format("Received set request for Tab: ~p, Row: ~p, Col: ~p, Val: ~p~n", [Tab, I, J, Val]),
                    if
                        Tab > length(Tabs) orelse Tab < 1 ->
                            io:format("Tab index ~p is out of bounds~n", [Tab]),
                            From ! {set_result, false};
                        true ->
                            TabMatrix = lists:nth(Tab, Tabs),
                            if
                                I > length(TabMatrix) orelse I < 1 ->
                                    io:format("Row index ~p is out of bounds in Tab ~p~n", [I, Tab]),
                                    From ! {set_result, false};
                                true ->
                                    Row = lists:nth(I, TabMatrix),
                                    if
                                        J > length(Row) orelse J < 1 ->
                                            io:format("Col index ~p is out of bounds in Row ~p, Tab ~p~n", [J, I, Tab]),
                                            From ! {set_result, false};
                                        true ->
                                            io:format("Setting value ~p at Tab: ~p, Row: ~p, Col: ~p~n", [Val, Tab, I, J]),
                                            NewRow = replace_nth(J, Val, Row),
                                            NewTabMatrix = replace_nth(I, NewRow, TabMatrix),
                                            NewTabs = replace_nth(Tab, NewTabMatrix, Tabs),
                                            NewState = State#spreadsheet{tabs = NewTabs},
                                            From ! {set_result, true},
                                            loop(NewState)
                                    end
                            end
                    end;
                {error, access_denied} ->
                    From ! {error, access_denied},
                    loop(State)
            end;                  

       



        {get_spreadsheetPid, From} ->
            
                io:format("Received request for spreadsheet state from: ~p~n", [From]),
                io:format("State to be returned: ~p~n", [State]),  % Stampa il valore di `State`
                % Invia il record #spreadsheet{} indietro
                From ! {spreadsheet_state, State},
                io:format("Sent spreadsheet state to: ~p~n", [From]),
                loop(State);  % Continua ad ascoltare messaggi

        {share, From, NewPolicies} ->
            if
                From =:= State#spreadsheet.owner ->
                    % Aggiornare le politiche di accesso usando update_policies/2
                    UpdatedPolicies = update_policies(NewPolicies, Policies),
                    NewState = State#spreadsheet{access_policies = UpdatedPolicies},
                    io:format("Updated access policies: ~p~n", [UpdatedPolicies]),  % Debug
                    From ! {share_result, true},
                    loop(NewState);
                true ->
                    % Se non è il proprietario, ritorna un errore
                    From ! {share_result, {error, not_owner}},
                    loop(State)
            end;
        {remove_policy, From, Proc} ->
            if
                From =:= Owner ->
                    
                    NewPolicies = lists:filter(fun({P, _}) -> P =/= Proc end, Policies),
                    NewState = State#spreadsheet{access_policies = NewPolicies},
                    From ! {remove_policy_result, ok},
                    loop(NewState);
          
                true ->
                    From ! {error, not_owner},
                    loop(State)
            end;
        stop -> 

            ok; % esce dal loop , ma attenzione non arresta Spreadsheet !!!
        _Other ->
            io:format("Unknown message received: ~p~n", [_Other]),
            loop(State)
    end.
% Funzione per salvare il foglio di calcolo in un file CSV, attraverso il registered name
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
            Pid ! {get_spreadsheetPid, self()},
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
%% Funzioni ausiliarie  to_csv/1
    % Funzione per salvare ogni tab come riga CSV
save_tab_to_csv(File, Tab) ->
    lists:foreach(fun(Row) ->
        Line = lists:map(fun(Cell) -> format_cell(Cell) end, Row),
        io:format(File, "~s~n", [string:join(Line, ",")])
    end, Tab).

    % Formattazione delle celle per CSV
format_cell(undef) -> "undef";
format_cell(Cell) -> io_lib:format("~p", [Cell]).


    % Funzione per caricare il foglio di calcolo da un file CSVhe function opens the file and reads the first line.
%If the first line starts with "Spreadsheet Name: ", we extract the actual name (SpreadsheetName) and proceed to load the rest of the data.
%If the format of the first line doesn’t match, we handle it gracefully by closing the file and returning an error.
from_csv(Filename) ->
    case file:open(Filename, [read]) of
        {ok, File} ->
            % Read the first line (Spreadsheet Name)
            SpreadsheetNameLine = io:get_line(File, ''),
            io:format("Raw Spreadsheet Name Line: ~p~n", [SpreadsheetNameLine]),  % Debug

            
            %The error variable 'SpreadsheetName' unsafe in 'case' occurs in Erlang when you attempt to bind a variable inside a
            % case expression, but then try to use it outside of the case block. Variables bound 
            %in a case expression are only valid within that expression, and Erlang does not allow them to be used outside of it.            case string:strip(SpreadsheetNameLine, both, $\n) of
            
            % Pattern match to extract the spreadsheet name
            case string:strip(SpreadsheetNameLine, both, $\n) of
                "Spreadsheet Name: " ++ SpreadsheetName ->  
                    io:format("Extracted Spreadsheet Name: ~p~n", [SpreadsheetName]),
                    
                    % Load the tabs from the remaining lines in the CSV
                    Tabs = load_tabs_from_csv(File),
                    io:format("Read Tabs: ~p~n", [Tabs]),  % Debug

                    file:close(File),

                    % Construct the spreadsheet record
                    Spreadsheet = #spreadsheet{
                        name = list_to_atom(SpreadsheetName),  % Convert the name to an atom
                        tabs = Tabs,
                        owner = self(),  % Set the current process as the owner
                        access_policies = []  % Empty access policies for now
                    },
                    {ok, Spreadsheet};

                _ ->
                    io:format("Error: Invalid format for Spreadsheet Name line~n"),
                    file:close(File),
                    {error, invalid_format}
            end;

        {error, Reason} ->
            io:format("Error opening file: ~p~n", [Reason]),
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




