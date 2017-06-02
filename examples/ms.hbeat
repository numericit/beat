
-define(AUTHOR, "bob").
-define(AUTHOR(A), {"bob", A}).
-define(V(Line, Type, Val), {val, Line, Type, Val}).
-define(Int(Val), ?V(_, integer, Val)).
-define(Int(Line, Val), ?V(Line, integer, Val)).
-define(Int1(Line, Val), 1 + ?V(Line, integer, Val)).
-define(OneFreeVar(Val), Val + FreeVar).
-define(AddPlusOne(A, B), 1 + A + B).
-define(Text(Val), ??Val).
-define(NestedText(A), A ++ ?Text(1 - 2)).
-define(Const, 2 * 3).
-define(Inc(A), A + 1).
-define(IncConst(A), A + ?Const).
-define(TESTCALL(Call), io:format("Call ~s: ~w~n", [??Call, Call])).

what_am_i() -> a_function_in_the_macro_file.
