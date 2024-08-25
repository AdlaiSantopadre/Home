-module(link).
-export([start/0, request/1, loop/0]).


%registriamo il servizio addone e usiamo la spawn_link invece di spawn + link%
start() ->
  register(add_one, spawn_link(link, loop, []))
.

request(Int) ->
  add_one ! {request, self(), Int},
  receive
    {result, Result} -> Result
    after 1000 -> timeout end
.

loop() ->
  receive
      {request, Pid, Msg} ->
      Pid ! {result, Msg + 1}
  end,
loop()
.
