-module(listfuncs).
-export([productd/1, productt/1, productt/2]).
-export([listmaxd/1, listmaxt/1, listmaxt/2]).

%
% Combining list elements: the product of a list
%

% Direct Recursion
productd([]) -> 1; % The multiplying by 0 gives 0, so need to return 1
productd([X|Xs]) -> X * productd(Xs).

% Tail Recursion
productt([]) -> 1;
productt(Xs) -> productt(Xs, 1).

productt([], Acc) -> Acc;
productt([X|Xs], Acc) -> productt(Xs, X*Acc).

%
% Combining list elements: the maximum of a list
%

% Direct recursion
%listmaxd([]) -> erlang:error(badarg);
listmaxd([X]) when X =/= [] -> X;
listmaxd([X|Xs]) -> (max(X, listmaxd(Xs))).

% Tail recursion
%listmaxt([]) -> erlang:error(badarg);
listmaxt(Xs) when Xs =/= [] -> listmaxt(Xs, 0).

listmaxt([X], Acc) -> max(Acc, X);
listmaxt([X|Xs], Acc) -> listmaxt(Xs, max(Acc, X)).
