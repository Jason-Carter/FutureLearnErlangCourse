-module(dedupe).
-export([exists/2,nubfb/1,nubfb/2,nubbf/1,nubbf/2,nub_test/0]).

% nubfb([2,4,1,3,3,1]) = [2,4,1,3]
% nubbf([2,4,1,3,3,1]) = [2,4,3,1]


exists(_, []) -> false;
exists(Val, [X|_Xs]) when Val == X -> true;
exists(Val, [X|Xs]) when Val =/= X -> exists(Val, Xs).

-spec nubfb([T]) -> [T].
nubfb(List) -> nubfb(List, []).

nubfb([], NewList) -> NewList;
nubfb([X|Xs], NewList) -> 
    case exists(X, NewList) of
        true -> nubfb(Xs, NewList);
        false -> nubfb(Xs, NewList ++ [X] )
    end.

-spec nubbf([T]) -> [T].
nubbf(List) -> nubbf(lists:reverse(List), []).

nubbf([], NewList) -> NewList;
nubbf([X|Xs], NewList) ->
    case exists(X, NewList) of
        true -> nubbf(Xs, NewList);
        false -> nubbf(Xs, [X] ++ NewList)
    end.

nub_test() ->
    [2,4,1,3] = nubfb([2,4,1,3,3,1]),
    [2,4,3,1] = nubbf([2,4,1,3,3,1]),
    nub_test_passed.