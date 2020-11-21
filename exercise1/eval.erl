-module(eval).
-compile(export_all).
-author("Max Shi").

extract_val({val, Value}) -> Value.

calc({_, Val}) -> {val, Val};
calc({add, Val1, Val2}) -> {val, extract_val(calc(Val1)) + extract_val(calc(Val2))};
calc({sub, Val1, Val2}) -> {val, extract_val(calc(Val1)) - extract_val(calc(Val2))};
calc({mult, Val1, Val2}) -> {val, extract_val(calc(Val1)) * extract_val(calc(Val2))};
calc({divi, Val1, Val2}) -> {val, extract_val(calc(Val1)) / extract_val(calc(Val2))}.


e1() ->
    {add, {const, 3}, {divi, {const, 4}, {const, 3}}}.

match_var(Desired) -> 
    F = fun({Var, _}) -> Desired == Var end.

calc2(add, Env) -> ;
calc2(var, Env) ->  {value, {_, Val}} = lists:search(match_var)
calc2(const, Env) -> ;
calc2(sub, Env) -> ;
calc2(mul, Env) -> ;
calc2(divi, Env) -> ;
calc2(val, Env) -> ;