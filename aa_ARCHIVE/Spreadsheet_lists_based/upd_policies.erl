
-module(upd_policies).
-export([update_policies/2]).
update_policies(NewPolicies, ExistingPolicies) ->
    % Step 1: Filter ExistingPolicies to exclude entries that are already in NewPolicies
    FilteredExistingPolicies = [
        Policy || 
        {Proc, _} = Policy <- ExistingPolicies, 
        not (
            lists:keymember(Proc, 1, NewPolicies) orelse
            is_pid(Proc) andalso lists:any(fun({NewProc, _}) -> whereis(NewProc) == Proc end, NewPolicies) orelse
            not is_pid(Proc) andalso lists:any(fun({NewProc, _}) -> NewProc == whereis(Proc) end, NewPolicies)
        )
    ],

    % Step 2: Return the combined list of NewPolicies and FilteredExistingPolicies
    FilteredExistingPolicies ++ NewPolicies.
    %With this single list comprehension, you can effectively:

    %Remove duplicates where the process in ExistingPolicies is already represented in NewPolicies (either as a PID or registered name).
    % Ensure that PIDs and registered names referring to the same process are handled correctly without introducing duplicates.
