-module(process_utility).
-export([spawn_and_register_processes/1]).

% Function to spawn and register processes based on a list of atom names
spawn_and_register_processes(Names) ->
    lists:foreach(fun(Name) ->
                      Pid = spawn(fun() -> process_loop() end),
                      register(Name, Pid)
                  end, Names).

% Simple process loop function that logs received messages
process_loop() ->
    receive
        Message ->
            io:format("Received message: ~p~n", [Message]),
            process_loop()
    end.
