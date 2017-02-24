-module(patterns).
-export([is_zero/1, xOr/2, xOr2/2, xOr3/2, xOr4/2, xOr5/2]).
-export([maxThree/3, howManyEqual/3]).

%
% The following functions are the examples provided in step 1.14
%
is_zero(0) ->
    true;
is_zero(_) ->
    false.

xOr(true,false) ->
    true;
xOr(false,true) ->
    true;
xOr(_,_) ->
    false.

xOr2(X,X) ->
    false;
xOr2(_,_) ->
    true.

%
% My version of the functions for exercise 1.15
% (using the operators suggested)
%
xOr3(X,Y) -> X =/= Y.

xOr4(X,Y) -> X == not Y.

xOr5(X,Y) -> X and not Y.

% (I prefer the pattern matching version in xOr2)


maxThree(X,Y,Z) -> max(X, max(Y, Z)).

howManyEqual(X, X, X) -> 3;
howManyEqual(_, X, X) -> 2;
howManyEqual(X, _, X) -> 2;
howManyEqual(X, X, _) -> 2;
howManyEqual(_, _, _) -> 0.

