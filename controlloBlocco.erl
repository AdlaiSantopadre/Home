%OK
-module(controlloBlocco).

-export([test1/0, provaBlock/0]).
-export([provaNoBlock/0]).

test1() ->
    Pid = spawn(controlloBlocco, provaBlock, []),
    lists:foreach(fun(X) ->
                        Pid ! {X},
                        io:format("sending ~p to ~p\n", [X,Pid])
                    end,
                  lists:seq(0, 10)),
    Pid ! {ping}.
%This part of the code uses lists:seq/2 to generate a list 
%of integers from 0 to 10 (inclusive). 
%The lists:foreach/2 function then iterates over each element of this list,
% sending {X} to the process identified
% by Pid and printing a corresponding message using io:format/2
provaBlock() ->
    receive
        {ping} ->
            io:format("Received ping~n", [])
    end.
provaNoBlock()->
Self = self(),
io:format("process ~p waiting for message\n",[Self]),
receive
{ping} -> io:format("process ~p got ping\n",[Self])
after 1000 -> io:format("no pings after 1sec\n")
end.

%If the process does not receive any {ping} message
% after 1sec (= 1000ms) it prints something and terminates
