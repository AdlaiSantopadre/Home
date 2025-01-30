{application, my_app,[
  {description, "Distributed Spreadsheet Application"},
  {vsn, "2.0"},
  {modules, [my_app, app_sup, spreadsheet_supervisor, distributed_spreadsheet]},
  {registered, [app_sup]},
  {applications, [kernel, stdlib, mnesia ]},  % Dipendenze richieste + mnesia
  {mod, {my_app, []}},
  {distributed, [{my_app, ['Alice@DESKTOPQ2A2FL7', 'Bob@DESKTOPQ2A2FL7', 'Charlie@DESKTOPQ2A2FL7']}]},
  {env, [{csv_directory, "/exported_csv"}]}
 ]}.
