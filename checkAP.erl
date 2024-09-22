-module(checkAP).
-export([check_access/3]).

check_access(PidOrName, Policies, RequiredAccess) ->

    io:format("control if ~p  process has access to ~p in list ~p~n",[PidOrName,RequiredAccess,Policies]),

    % Resolve PidOrName to both PID and registered name if possible
    ResolvedPid = case is_pid(PidOrName) of
                     true -> PidOrName;
                     false -> Resolved = whereis(PidOrName),
                                io:format("PidOrName is a registered name resolving to: ~p~n", [Resolved]),
                               Resolved
                  end,
    % Get the registered name if PidOrName is a PID and registered
    RegisteredName = case is_pid(PidOrName) of
                         true -> lists:keyfind(PidOrName, 2, erlang:processes());  % Check if it's registered
                         false -> PidOrName  % If it's already a name, keep it
                     end,
    io:format("ResolvedPid is ~p~n",[ResolvedPid]),io:format("registeredName is  ~p~n",[RegisteredName]),
    % Check if either the resolved PID or the registered name has the required access in the access policies
    case lists:keyfind(ResolvedPid, 1, Policies) of
        {ResolvedPid, Access} when Access == RequiredAccess ->io:format("tuple is ~p~n",[{ResolvedPid,Access}]),
            ok;
            _ ->               
                case lists:keyfind(RegisteredName, 1, Policies) of
                    {RegisteredName, Access} when Access == RequiredAccess -> io:format("tuple is ~p~n",[{RegisteredName,Access}]), ok;
                 _ -> {error, access_denied}
            end
    end.