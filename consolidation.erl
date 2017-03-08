-module(consolidation).
-export([join/2, join_test/0]).
-export([concat/1, concat_test/0]).
-export([member/2]).

% Joining Lists
%
% My implementation of ++ called join/2 (which uses shunt from previous modules)

-spec join([T], [T]) -> [T].
join([],[]) -> [];
join(X, []) -> X;
join([], Y) -> Y;
join(X, Y)  -> join(X, Y, []).

join(X, Y, ReverseX) -> join([], shunt(shunt(X, ReverseX), Y)).

shunt([],Ys)     -> Ys;
shunt([X|Xs],Ys) -> shunt(Xs,[X|Ys]).

join_test() ->
    "hello"     = join("hel", "lo"),
    [5,4,3,2,1] = join([5,4], [3,2,1]),
    test_join_passed.

% My implementation of lists:concat
-spec concat([T]) -> [T].
% Not sure if [T] is correct since it's supposed to be a list of lists

concat(List) -> concat(List, []).

concat([], Result)  -> Result;
concat([ListH | ListT], Result) -> concat(ListT, join(Result, ListH)).

concat_test() ->
    "goodbye" = concat(["goo","d","","by","e"]),
    [1,2,3,4,5,6] = concat([[1,2], [3,4,5], [6]]),
    concat_test_passed.

% Testing membership
-spec member(T, [T]) -> boolean().

member(_, []) -> false;
member(Elem, [Elem | _ListT]) -> true;
member(Elem, [_ListH | ListT]) -> member(Elem, ListT).

% Sorting lists
%
%TODO: Will return to this when I have more time



% Permutations
%
%TODO: Will return to this when I have more time

