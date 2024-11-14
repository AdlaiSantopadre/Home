-record(spreadsheet, {
    name,                % Nome del foglio di calcolo
    tabs = [],           % Dati (una lista di tab, una tab è una matrice)
    owner = undefined,   % Proprietario del foglio di calcolo (PID del processo creatore)
    access_policies = [], % Politiche di accesso (lista di tuple)
    last_modified = undefined   % Timestamp dell` ultima modifica dei dati
}).