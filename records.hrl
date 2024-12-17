-record(spreadsheet_data, {
    name,                   % Nome univoco del foglio di calcolo
    tab,                    % indice di accesso alla tabella 
    row,                    %indice di accesso alla riga
    col,                    %indice di accesso alla colonna
    value                   %valore della cella
    
}).
-record(spreadsheet_owners, {
    name,                   % Nome univoco del foglio di calcolo
    owner                  % pid del possessore    
}).
-record(access_policies, {
    name,                   % Nome univoco del foglio di calcolo
    proc,                  % pid o registered name del processo
    access                  % accesso read o write
}).
-record(state, {owners = []}). % Lista di tuple {SpreadsheetName, OwnerPid, Ref}


