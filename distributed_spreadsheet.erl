-module(distributed_spreadsheet).
%% gen_server with mnesia
-behaviour(gen_server).

%% Include the record definitions
-include("records.hrl").

%% API functions exported from assignement

-export([new/1, new/4, share/2, get/4, get/5, set/5, set/6, info/1, to_csv/3, to_csv/2,from_csv/1, from_csv/2]).
%% API
-export([start_link/1]).

%%REMOVE AFTER TEST
%update_access_policies/3,
-export([find_global_name/1, check_access/3,parse_value/1,read_from_csv/1]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                    API FUNCTIONS                   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Crea uno spreadsheet con valori di default
new(Name) ->
    N = 3,
    M = 4,
    K = 2,
    % Default N (numero di righe), M (numero di colonne), K (numero di tab)
    new(Name, N, M, K).

%% Crea uno spreadsheet con parametri specifici
new(SpreadsheetName, N, M, K) ->
    %% Richiesta del pid del master di my_app
    OwnerPid = application_controller:get_master(my_app),
    Args = {SpreadsheetName, N, M, K, OwnerPid},
    case spreadsheet_supervisor:start_spreadsheet(Args) of
        %% Invio richiesta a spreadsheet_supervisor per creare il supervisore specifico
        {ok, Pid} ->
            io:format("Spreadsheet ~p started successfully with PID ~p~n", [SpreadsheetName, Pid]),
            {ok, Pid};
        {error, Reason} ->
            io:format("Failed to start spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
            {error, Reason}
    end.

%% Permette di avere informazioni sugli spreadsheets
info(SpreadsheetName) ->
    %% Richiesta del pid del master di my_app

    %% Step 1: Check if the process is registered globally
    case global:whereis_name(SpreadsheetName) of
        undefined ->
            io:format("Spreadsheet ~p not found globally.~n", [SpreadsheetName]),
            {error, spreadsheet_not_found};
        Pid when is_pid(Pid) ->
            %% Step 2: Check if the process is alive
            OwnerPid = application_controller:get_master(my_app),
            io:format("Spreadsheet ~p is registered globally with PID ~p~n", [SpreadsheetName, Pid]),

            try
                gen_server:call(Pid, {about, SpreadsheetName, OwnerPid})
            catch
                Class:Reason ->
                    io:format("Error calling gen_server:call/2 with Pid: ~p, Reason: ~p, ~p~n", [
                        Pid, Class, Reason
                    ]),
                    {error, {call_failed, Reason}}
            end
    end.
%% Permette l'accesso condiviso allo spreadsheet secondo una lista di politiche di accesso
share(SpreadsheetName, AccessPolicies) when is_list(AccessPolicies) ->
    case global:whereis_name(SpreadsheetName) of
        undefined ->
            % If the process is not found
            {error, spreadsheet_not_found};
        Pid when is_pid(Pid) ->
            %%MasterPid= application_controller:get_master(my_app),
            gen_server:call({global, SpreadsheetName}, {share, SpreadsheetName, [AccessPolicies]})
    end.
%% API function to get the value from a specific cell with default timeout (infinity)
get(SpreadsheetName, TabIndex, I, J) ->
    get(SpreadsheetName, TabIndex, I, J, infinity).

%% API function to get the value from a specific cell with a custom timeout
get(SpreadsheetName, TabIndex, I, J, Timeout) when
    is_integer(TabIndex), is_integer(I), is_integer(J)
->
    case global:whereis_name(SpreadsheetName) of
        undefined ->
            {error, spreadsheet_not_found};
        Pid when is_pid(Pid) ->
            MasterPid = application_controller:get_master(my_app),
            %% Make a gen_server:call with a timeout
            try
                io:format("Caller node with MasterPid: ~p~n", [MasterPid]),
                gen_server:call(Pid, {get, SpreadsheetName, TabIndex, I, J, MasterPid}, Timeout)
            catch
                _:_ -> {error, timeout}
            end
    end.
%% API function to set a value with default timeout (infinity)
set(SpreadsheetName, TabIndex, I, J, Value) ->
    set(SpreadsheetName, TabIndex, I, J, Value, infinity).

%% API function to set a value with a custom timeout
set(SpreadsheetName, TabIndex, I, J, Value, Timeout) when
    is_integer(TabIndex), is_integer(I), is_integer(J)
->
    case global:whereis_name(SpreadsheetName) of
        undefined ->
            {error, spreadsheet_not_found};
        Pid when is_pid(Pid) ->
            MasterPid = application_controller:get_master(my_app),
            %% Check if value is a valid Erlang term, dynamically handle all types
            case validate_value(Value) of
                ok ->
                    try
                        gen_server:call(
                            Pid, {set, SpreadsheetName, TabIndex, I, J, MasterPid, Value}, Timeout
                        )
                    catch
                        _:_ -> {error, timeout}
                    end;
                {error, invalid_type} ->
                    {reply, {error, invalid_type}}
            end
    end.

%%% Esporta i dati dello spreadsheet in un file CSV

to_csv(SpreadsheetName, Filename) ->
    to_csv(SpreadsheetName, Filename, infinity).

to_csv(SpreadsheetName, Filename, Timeout) ->
    gen_server:call({global, SpreadsheetName}, {to_csv, SpreadsheetName, Filename}, Timeout).

%%% Importa i dati da un file CSV e li inserisce nella tabella spreadsheet_data.
from_csv(Filename) ->  
    from_csv(Filename, infinity).
from_csv(Filename, Timeout) ->
    %% Recupera la directory configurata
    %%CsvDirectory = application:get_env(my_app, csv_directory, "/default/path"),
    %%FullPath = filename:join(CsvDirectory, Filename),
    %%io:format("FullPath: ~p~n", [FullPath]),
%% recupero il nome globale dello spreadshheet
    case file:open(Filename, [read]) of
            {ok, IoDevice} ->
                case io:get_line(IoDevice, '') of
                    "Spreadsheet Name: " ++ SpreadsheetNameLine ->
                    Name = string:strip(SpreadsheetNameLine, both, $\n),
                    SpreadsheetName=parse_value(Name),
                    file:close(IoDevice)
                end
            end,
    gen_server:call({global,SpreadsheetName}, {from_csv, Filename}, Timeout).

%% Avvia il gen_server /registra il nome globalmente
start_link(Args) ->
    io:format("Starting distributed_spreadsheet with args: ~p~n", [Args]),
    {SpreadsheetName, _, _, _, _} = Args,
    gen_server:start_link({global, SpreadsheetName}, ?MODULE, Args, []).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Inizializza il processo spreadsheet distribuito
init({SpreadsheetName, N, M, K, OwnerPid}) ->
    io:format("Initializing spreadsheet ~p with owner ~p~n", [SpreadsheetName, OwnerPid]),

    %% Controlla se esiste già un proprietario per lo spreadsheet
    case mnesia:transaction(fun() -> mnesia:read(spreadsheet_info, SpreadsheetName) end) of
        {atomic, []} ->
            %% Creazione di un nuovo spreadsheet
            io:format("No existing data for spreadsheet ~p. Initializing records.~n", [
                SpreadsheetName
            ]),
            Records = generate_records(SpreadsheetName, N, M, K),
            mnesia:transaction(fun() ->
                %% Scrive i dati e le info nelle tabelle
                lists:foreach(fun(Record) -> mnesia:write(Record) end, Records),
                %%mnesia:write(#spreadsheet_owners{name = SpreadsheetName, owner = OwnerPid}),
                mnesia:write(#spreadsheet_info{
                    name = SpreadsheetName, rows = N, cols = M, tabs = K, owner = OwnerPid
                })
            end),
            {ok, #{name => SpreadsheetName, size => {N, M, K}, owner => OwnerPid}};
        {atomic, [
            #spreadsheet_info{
                name = SpreadsheetName, rows = N, cols = M, tabs = K, owner = ExistingOwner
            }
        ]} ->
            %% Caso di ripristino
            if
                ExistingOwner == OwnerPid ->
                    io:format("Found existing owner ~p for spreadsheet ~p.~n", [
                        OwnerPid, SpreadsheetName
                    ]),
                    {ok, #{name => SpreadsheetName, size => {N, M, K}, owner => ExistingOwner}};
                true ->
                    %% Riassegna il proprietario
                    io:format("Reassigning ownership of spreadsheet ~p to PID ~p~n", [
                        SpreadsheetName, OwnerPid
                    ]),
                    mnesia:transaction(fun() ->
                        mnesia:delete_object(#spreadsheet_info{
                            name = SpreadsheetName,
                            rows = N,
                            cols = M,
                            tabs = K,
                            owner = ExistingOwner
                        }),
                        mnesia:write(#spreadsheet_info{
                            name = SpreadsheetName, rows = N, cols = M, tabs = K, owner = OwnerPid
                        })
                    end),
                    {ok, #{name => SpreadsheetName, size => {N, M, K}, owner => OwnerPid}}
            end;
        {aborted, Reason} ->
            io:format("Failed to initialize spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
            {stop, Reason}
    end.

% init({SpreadsheetName, N, M, K, OwnerPid}) ->
%     io:format("Initializing spreadsheet ~p with owner ~p~n", [SpreadsheetName, OwnerPid]),

%     case
%         mnesia:transaction(fun() ->
%             case mnesia:read({spreadsheet_owners, SpreadsheetName}) of
%                 %% New spreadsheet creation
%                 [] ->
%                     io:format("No existing data for spreadsheet ~p. Initializing records.~n", [
%                         SpreadsheetName]),

%                     Records = generate_records(SpreadsheetName, N, M, K),
%                     lists:foreach(fun(Record) -> mnesia:write(Record) end, Records),
%                     mnesia:write(#spreadsheet_owners{name = SpreadsheetName, owner = OwnerPid}),
%                     mnesia:write(#spreadsheet_info{name = SpreadsheetName, rows = N, cols = M, tabs = K}),
%                     {new, ok};
%                 %% Restart case
%                 [#spreadsheet_owners{name = SpreadsheetName, owner = ExistingOwner}] ->
%                     if  ExistingOwner == OwnerPid -> io:format("Found existing owner ~p for spreadsheet ~p.~n", [OwnerPid, SpreadsheetName ]),
%                     {existing, ExistingOwner};
%                     true -> mnesia:delete_object(#spreadsheet_owners{name = SpreadsheetName, owner = OwnerPid}),
%                             io:format("Reassign ownership of spreadsheet ~p to pid ~p .~n", [SpreadsheetName,OwnerPid]),
%                             mnesia:write(#spreadsheet_owners{name = SpreadsheetName, owner = OwnerPid}),
%                             {existing, ExistingOwner}
%                             %%mnesia:write(#spreadsheet_info{name = SpreadsheetName, rows = N, cols = M, tabs = K}),
%                     end
%                 end

%         end)
%     of
%         {atomic, {new, ok}} ->
%             %% oK
%             {ok, #{name => SpreadsheetName, size => {N, M, K}, owner => OwnerPid}};
%         {atomic, {existing, ExistingOwner}} ->
%             {ok, #{name => SpreadsheetName, size => {N, M, K}, owner => ExistingOwner}};
%         {aborted, Reason} ->
%             io:format("Failed to initialize spreadsheet ~p: ~p~n", [SpreadsheetName, Reason]),
%             {stop, Reason}
%     end.

% %% Handle the 'share' request in the gen_server
% handle_call({share, SpreadsheetName, AccessPolicies}, {FromPid, _Alias}, State) ->
%     io:format("Received share/2 for spreadsheet ~p from Pid ~p ~n", [SpreadsheetName, FromPid]),
%     %% Verifica se il chiamante è il proprietario dello spreadsheet
%     MasterPid= application_controller:get_master(my_app),
%     case mnesia:transaction(fun() ->
%         case mnesia:read({spreadsheet_owners,SpreadsheetName}) of
%             [#spreadsheet_owners{owner = MasterPid}] -> ok;
%             _ -> {error, unauthorized}
%         end
%     end) of
%         {atomic, ok} ->
%              %% Step 1: Recupera le politiche esistenti
%             ExistingPolicies = [
%                 {Policy#access_policies.proc, Policy#access_policies.access}
%                 || Policy <- mnesia:match_object(#access_policies{name = SpreadsheetName, proc = '_', access = '_'})
%             ],
%             io:format("Existing Policies: ~p~n", [ExistingPolicies]),
%             case update_access_policies(SpreadsheetName, AccessPolicies,ExistingPolicies) of
%                 {atomic, ok} ->
%                     io:format("Access policies for ~p updated successfully.~n", [SpreadsheetName]),
%                     {reply, ok, State};
%                 {error, Reason} ->
%                     io:format("Failed to update access policies for ~p: ~p~n", [SpreadsheetName, Reason]),
%                     {reply, {error, Reason}, State}
%             end;
%         {atomic, {error, unauthorized}} ->
%             io:format("Unauthorized access to update policies for ~p by ~p.~n", [SpreadsheetName, FromPid]),
%             {reply, {error, unauthorized}, State};
%         {aborted, Reason} ->
%             io:format("Transaction failed while verifying ownership for ~p: ~p~n", [SpreadsheetName, Reason]),
%             {reply, {error, Reason}, State}
%     end;
%% Handle the synchronous request to get the spreadsheet's info
%%%%%%%%%%%%%%%%%HANDLE CALL ABOUT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({about, SpreadsheetName, MasterPid}, _From, State) ->
    io:format("getting info about  ~p~n", [SpreadsheetName]),
    case
        mnesia:transaction(fun() ->
            mnesia:match_object(#spreadsheet_info{
                name = SpreadsheetName, rows = '_', cols = '_', tabs = '_', owner = MasterPid
            })
        end)
    of
        {atomic, [
            #spreadsheet_info{name = Name, rows = Rows, cols = Cols, tabs = Tabs, owner = Owner}
        ]} ->
            %%numero totale delle celle
            TotalCells = Rows * Cols,

            %% Recupera le politiche di accesso dalla tabella access_policies

            case
                mnesia:transaction(fun() ->
                    mnesia:match_object(#access_policies{name = Name, proc = '_', access = '_'})
                end)
            of
                {atomic, Policies} ->
                    %% Filtra i permessi
                    ReadPermissions = [
                        {Policy#access_policies.proc, Policy#access_policies.access}
                     || Policy <- Policies, Policy#access_policies.access =:= read
                    ],
                    io:format("ReadPermissions: ~p~n", [ReadPermissions]),
                    WritePermissions = [
                        {Policy#access_policies.proc, Policy#access_policies.access}
                     || Policy <- Policies, Policy#access_policies.access =:= write
                    ],

                    %% Create the info result map
                    Info = #{
                        name => Name,
                        owner => Owner,
                        %last_modified => LastModified,
                        total_tabs => Tabs,
                        total_cells => TotalCells,
                        read_permissions => ReadPermissions,
                        write_permissions => WritePermissions
                    },
                    {reply, {ok, Info}, State};
                {aborted, Reason} ->
                    io:format("Transaction aborted: ~p~n", [Reason]),
                    {error, transaction_aborted}
            end;
        {atomic, []} ->
            %% Nessuno spreadsheet trovato
            {reply, {error, spreadsheet_not_found}, State};
        {aborted, Reason} ->
            %% Gestisce errori di transazione
            io:format("Transaction aborted: ~p~n", [Reason]),
            {reply, {error, transaction_aborted}, State}
    end;
%%%%%%%%%%HANDLE %%%%%%%%% GET %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Handle the 'get' request in the gen_server SpreadsheetName, N, M, K, OwnerPid
handle_call({get, SpreadsheetName, TabIndex, I, J, MasterPid}, _From, State) ->
    CallerPid = MasterPid,
    io:format("Get request from ~p for Tab: ~p, Row: ~p, Col: ~p~n", [CallerPid, TabIndex, I, J]),

    %% Check if the calling process has read access (or superior write access)
    case check_access(CallerPid, [read, write], SpreadsheetName) of
        ok ->
            io:format("Returning value for Tab: ~p, Row: ~p, Col: ~p~n", [TabIndex, I, J]),

            case
                mnesia:transaction(fun() ->
                    case
                        %   mnesia:match_object(#access_policies{name = SpreadsheetName, proc = '_', access = '_'})
                        mnesia:match_object(#spreadsheet_data{
                            name = SpreadsheetName, tab = TabIndex, row = I, col = J, value = '_'
                        })
                    of
                        [undef] -> undef;
                        [#spreadsheet_data{value = Value}] -> Value
                    end
                end)
            of
                {atomic, Value} ->
                    io:format("Returning value for Tab: ~p, Row: ~p, Col: ~p: ~p~n", [
                        TabIndex, I, J, Value
                    ]),
                    {reply, {ok, Value}, State};
                {aborted, Reason} ->
                    io:format("Transaction aborted for get request: ~p~n", [Reason]),
                    {reply, {error, transaction_aborted}, State}
            end;
        {error, access_denied} ->
            io:format("Access denied for process ~p~n", [CallerPid]),
            {reply, {error, access_denied}, State}
    end;
%%%%%%%%%%HANDLE %%%%%%%%% SET %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle the 'set' request in the gen_server
handle_call({set, SpreadsheetName, TabIndex, I, J, MasterPid, Value}, _From, State) ->
    CallerPid = MasterPid,
    io:format("Set request from ~p for Tab: ~p, Row: ~p, Col: ~p, Value: ~p~n", [
        CallerPid, TabIndex, I, J, Value
    ]),

    %% Check if the calling process has write access
    case check_access(CallerPid, [write], SpreadsheetName) of
        ok ->
            case
                mnesia:transaction(fun() ->
                    %% Recupera il record presente dalla tabella mnesia
                    mnesia:delete_object(#spreadsheet_data{
                        name = SpreadsheetName,
                        tab = TabIndex,
                        row = I,
                        col = J
                    }),

                    mnesia:write(#spreadsheet_data{
                        name = SpreadsheetName,
                        tab = TabIndex,
                        row = I,
                        col = J
                    })
                end)
            of
                {atomic, ok} ->
                    io:format("Returning value for Tab: ~p, Row: ~p, Col: ~p: ~p~n", [
                        TabIndex, I, J, Value
                    ]),
                    {reply, {ok, Value}, State};
                {aborted, Reason} ->
                    io:format("Transaction aborted for get request: ~p~n", [Reason]),
                    {reply, {error, transaction_aborted}, State}
            end;
        {error, access_denied} ->
            io:format("Access denied for process ~p~n", [CallerPid]),
            {reply, {error, access_denied}, State}
    end;
%%%%%%%%%%%%%HANDLE CALL TO_CSV%%%%%%%%%%%%%%%%%%%%
handle_call({to_csv, SpreadsheetName, Filename}, _From, State) ->
    %% Verifica che il nome dello spreadsheet sia corretto
    Name = maps:get(name, State),
    if
        Name =:= SpreadsheetName ->
            io:format("Exporting spreadsheet ~p to file ~p~n", [SpreadsheetName, Filename]),
            %% Legge tutti i record relativi allo spreadsheet
            case
                mnesia:transaction(fun() ->
                    mnesia:match_object(#spreadsheet_data{
                        name = SpreadsheetName, tab = '_', row = '_', col = '_', value = '_'
                    })
                end)
            of
                {atomic, Records} ->
                    %% Scrive i record nel file CSV
                    case write_csv(Filename, SpreadsheetName, Records)  of
                        ok ->
                            io:format("Spreadsheet ~p exported successfully to ~p~n", [
                                SpreadsheetName, Filename
                            ]),
                            {reply, ok, State};
                        {error, Reason} ->
                            io:format("Failed to write CSV for spreadsheet ~p: ~p~n ", [
                                SpreadsheetName, Reason
                            ]),
                            {reply, {error, Reason}, State}
                    end;
                {aborted, Reason} ->
                    io:format("Transaction aborted during export: ~p~n", [Reason]),
                    {reply, {error, transaction_aborted}, State}
            end;
        true ->
            {reply, {error, spreadsheet_not_found}, State}
    end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%HANDLE CALL FROM_CSV %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call({from_csv, NameorFullPath}, _From, State) ->
    %% Leggi il file CSV
    case read_from_csv(NameorFullPath) of
        {ok, Records} ->
            %% Salva i record nello spreadsheet
            case mnesia:transaction(fun() ->
                %% Scrive i dati e le info nelle tabelle
                lists:foreach(fun(Record) -> mnesia:write(Record) end, Records)
                
            end) of
                {atomic,ok} ->
                    {reply, ok, State};
                {aborted, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%     %% Legge i dati dal file CSV
%     case read_from_csv(Filename) of
%         {ok, Records} ->
%             %% Inserisce i record nella tabella mnesia
%             case
%                 mnesia:transaction(fun() ->
%                     lists:foreach(
%                         fun({Tab, Row, Col, Value}) ->
%                             mnesia:write(#spreadsheet_data{
%                                 name = SpreadsheetName,
%                                 tab = Tab,
%                                 row = Row,
%                                 col = Col,
%                                 value = Value
%                             })
%                         end,
%                         Records
%                     )
%                 end)
%             of
%                 {atomic, ok} ->
%                     io:format("Spreadsheet ~p imported successfully from ~p~n", [
%                         SpreadsheetName, Filename
%                     ]),
%                     {reply, ok, State};
%                 {aborted, Reason} ->
%                     io:format("Transaction aborted during import: ~p~n", [Reason]),
%                     {reply, {error, transaction_aborted}, State}
%             end;
%         {error, Reason} ->
%             io:format("Failed to read CSV from ~p: ~p~n", [Filename, Reason]),
%             {reply, {error, Reason}, State}
%     end;
handle_call(_Request, _From, State) ->
    {reply, {error, unsupported_operation}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
terminate(Reason, State) ->
    io:format("Terminating spreadsheet ~p with reason: ~p~n", [maps:get(name, State), Reason]),
    ok.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%% Funzione Helper write_to_csv/2%%%%%%%%%%%%%%
write_csv(Filename, SpreadsheetName, Records) ->
     try 
     File = Filename ++ ".csv",
    %% Apre il file in modalità scrittura
     file:open(File, [write]) of
        {ok, IoDevice} ->
           
                %% Scrive l'intestazione
                io:format(IoDevice, "Spreadsheet Name: ~s~n", [SpreadsheetName]),
                file:write(IoDevice, "Tab,Row,Col,Value\n"),
                
                %% Scrive i record
                lists:foreach(
                    fun(#spreadsheet_data{tab = Tab, row = Row, col = Col, value = Value}) ->
                        %% Genera la linea CSV
                        Line = io_lib:format("~p,~p,~p,~p\n", [Tab, Row, Col, Value]),
                        file:write(IoDevice, Line)
                    end,
                    Records
                ),
                file:close(IoDevice),
                ok
            catch
            _:_ -> {error, write_failed}

            end.
        


%%%%%%Funzione Helper read_from_csv/1 %%%%%%%%%%%%%%%%%%%%%%%%
read_from_csv(Filename) ->
    %try
        case file:open(Filename, [read]) of
            {ok, IoDevice} ->
                case io:get_line(IoDevice, '') of
                    "Spreadsheet Name: " ++ SpreadsheetNameLine ->
                    Name = string:strip(SpreadsheetNameLine, both, $\n)
                end,
                %% Salta l'intestazione CSV
                
                io:get_line(IoDevice, ''),
                case read_lines(IoDevice,[])  of
                    {ok, Lines}->
                            io:format("Lines: ~p~n", [Lines]),
                            Records = lists:map(fun(Line) ->
                            case string:tokens(string:strip(Line, both, $\n), ",") of
                                [Tab, Row, Col, ValueStr] ->
                                 #spreadsheet_data{name =list_to_atom(Name),
                                     tab=list_to_integer(Tab),
                                     row=list_to_integer(Row),
                                     col=list_to_integer(Col),
                                     value = parse_value(ValueStr)};
                                _ -> throw({error, invalid_csv_format})
                            end
                                end, Lines),
                        file:close(IoDevice),
                        {ok, Records};
                    {error, Reason} ->
                        {error, Reason}
                end
    %    end
   % catch
   %     _:_ -> {error, read_failed}
    end.

read_lines(IoDevice, Acc) ->

    case io:get_line(IoDevice, '') of
        eof -> {ok, lists:reverse(Acc)};
        Line -> read_lines(IoDevice, [Line | Acc])
        
    end.

%%%%%%Funzione Helper parse_value/1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_value(ValueString) ->
    case erl_scan:string(ValueString ++ ".") of  % Aggiungi un punto finale per termini completi
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;  % Parsing riuscito
                {error, Reason} -> {error, Reason}  % Parsing fallito
            end;
        {error, Reason, _} ->
            {error, Reason}  % Errore nella scansione
    end.




%genera i record per "rappresentare" lo spreadsheet usando list comprehensions.


generate_records(Name, N, M, K) ->
    [
        #spreadsheet_data{name = Name, tab = Tab, row = Row, col = Col, value = undef}
     || Tab <- lists:seq(1, K),
        Row <- lists:seq(1, N),
        Col <- lists:seq(1, M)
    ].

% update_access_policies(SpreadsheetName, ExistingPolicies, NewPolicies) ->
%     %% Preelabora NewPolicies per creare una mappa di PID e nomi globali
%     ResolvedNewPolicies = resolve_policies(NewPolicies),

%     %% Filtra le ExistingPolicies
%     FilteredExistingPolicies = lists:filter(fun({Proc, _}) ->
%         %% Risolvi Proc in ExistingPolicies
%         case resolve_to_global_or_pid(Proc) of
%             {ok, ResolvedProc} ->
%                 %% Mantieni solo se ResolvedProc è presente in ResolvedNewPolicies
%                 maps:is_key(ResolvedProc, ResolvedNewPolicies);
%             _ -> false
%         end
%     end, ExistingPolicies),

%     %% Combina le politiche filtrate con NewPolicies
%     UpdatedPolicies = FilteredExistingPolicies ++ NewPolicies,

%     %% Aggiorna la tabella mnesia
%     mnesia:transaction(fun() ->
%         %% Rimuovi le vecchie politiche
%         mnesia:delete({access_policies, SpreadsheetName}),
%         %% Inserisci le nuove politiche
%         lists:foreach(fun({Proc, Access}) ->
%             Record = #access_policies{name = SpreadsheetName, proc = Proc, access = Access},
%             io:format("Inserting Record: ~p~n", [Record]),
%             mnesia:write(Record)
%         end, UpdatedPolicies),
%         ok
%     end).
% %%   Verifica della politica di accesso in lettura
check_access(CallerPid, RequiredAccessList, SpreadsheetName) ->
    %% Trova il nome globale associato al CallerPid
    case find_global_name(CallerPid) of
        undefined ->
            io:format("nessun nome globale trovato per il chiamante ~p~n", [CallerPid]),
            {error, access_denied};
        GlobalName ->
            io:format("controlla se ~p ha il requisito ~n", [GlobalName]),
            case
                mnesia:transaction(fun() ->
                    %% Pattern per mnesia:match_object/1
                    Pattern = #access_policies{
                        name = SpreadsheetName, proc = GlobalName, access = '_'
                    },
                    Records = mnesia:match_object(Pattern),
                    %% Filtra i record con il permesso richiesto
                    lists:filter(
                        fun(#access_policies{access = Access}) ->
                            lists:member(Access, RequiredAccessList)
                        end,
                        Records
                    )
                end)
            of
                {atomic, []} ->
                    %% Nessun record soddisfa i requisiti
                    io:format("RequiredAccessList: ~p~n", [RequiredAccessList]),
                    {error, access_denied};
                {atomic, MatchingRecords} ->
                    %% Almeno un record soddisfa i requisiti
                    io:format("Selezionati questi record: ~p~n", [MatchingRecords]),
                    ok;
                {aborted, Reason} ->
                    %% Transazione abortita
                    io:format("Transaction aborted in check_access: ~p~n", [Reason]),
                    {error, transaction_aborted}
            end
    end.
%% Helper function to validate the type of Value
validate_value(Value) ->
    case is_basic_type(Value) of
        true -> ok;
        false -> {error, invalid_type}
    end.

%% Helper function to check for basic types in Erlang
is_basic_type(Value) when
    is_integer(Value);
    is_float(Value);
    is_atom(Value);
    is_list(Value);
    is_tuple(Value);
    is_map(Value);
    is_binary(Value);
    is_pid(Value)
->
    true;
is_basic_type(_) ->
    false.

%% Helper per trovare il nome globale associato a un CallerPid
find_global_name(CallerPid) ->
    %% Recupera tutti i nomi globali
    GlobalNames = global:registered_names(),
    io:format("Lista dei nomi globali registrati: ~p~n", [GlobalNames]),
    %% Trova il nome il cui PID corrisponde al CallerPid
    case
        lists:filter(
            fun(Name) ->
                case global:whereis_name(Name) of
                    CallerPid -> true;
                    _ -> false
                end
            end,
            GlobalNames
        )
    of
        %% Restituisce il primo nome globale trovato
        [GlobalName | _] -> GlobalName;
        %% Nessun nome globale trovato
        [] -> undefined
    end.
