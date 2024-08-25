filtrapari(X) ->
case X of
    [H | T] ->
    case (H rem 2) of
    0 -> [H] ++ filtrapari(T);
    1 -> filtrapari(T)
    end;
    [] -> []
end 
    .

% doc su liste di Erlang


