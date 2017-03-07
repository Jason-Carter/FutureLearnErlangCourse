-module(take).
-export([take/2, take_test/0]).

-spec take(integer(), [T]) -> [T].
take(0, _) -> [];
take(_, []) -> [];
take(Cnt, [X|Xs]) -> [X | take(Cnt-1, Xs)].

take_test() ->
    [] = take(0,"hello"),
    "hell" = take(4,"hello"),
     "hello" = take(5,"hello"),
     "hello" = take(9,"hello"),
    take_test_passed.

