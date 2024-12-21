{application, my_app,[
  {description, "Distributed Spreadsheet Application"},
  {vsn, "1.0"},
  {modules, [my_app, app_sup, spreadsheet_supervisor, distributed_spreadsheet]},
  {registered, [app_sup]},
  {applications, [kernel, stdlib, mnesia ]},  % Dipendenze richieste + mnesia
  {mod, {my_app, []}},
  {distributed, [{my_app, ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7']}]},
  {env, []} 
  %{path, ["ebin"]} %per spostare tutti i file compilati in ebin
 ]}.
