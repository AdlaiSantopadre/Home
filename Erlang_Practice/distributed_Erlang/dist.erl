-module(dist).
-export([s/0]).
s() -> register(server,self()),loop().
loop()->receive{M,Pid}
                ->Pid ! M end,
                loop().