-module(dedupe2).
-export([exists/2,nubfb/1,nubfb/2,nubbf/1,nubbf/2,nub_test/0]).

% nubfb([2,4,1,3,3,1]) = [2,4,1,3]
% nubbf([2,4,1,3,3,1]) = [2,4,3,1]


exists(Val, []) -> [Val];
exists(Val, [X|_Xs]) when Val == X -> [];
exists(Val, [X|Xs]) when Val =/= X -> exists(Val, Xs).

-spec nubfb([T]) -> [T].
nubfb(List) -> nubfb(List, []).

nubfb([], NewList) -> NewList;
nubfb([X|Xs], NewList) -> nubfb(Xs, NewList ++ exists(X, NewList)).

-spec nubbf([T]) -> [T].
nubbf(List) -> nubbf(lists:reverse(List), []).

nubbf([], NewList) -> NewList;
nubbf([X|Xs], NewList) -> nubbf(Xs, exists(X, NewList) ++ NewList).


nub_test() ->
    [2,4,1,3] = nubfb([2,4,1,3,3,1]),
    [2,4,3,1] = nubbf([2,4,1,3,3,1]),
    nub_test_passed.