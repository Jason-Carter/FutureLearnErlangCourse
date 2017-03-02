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
productt(Xs) -> productt(Xs, 1).

% Why???
% ** exception error: an error occurred when evaluating an arithmetic expression
%     in function  listfuncs:productt/2 (listfuncs.erl, line 14)
productt([], Acc) -> Acc;
productt([X|Xs], Acc) -> productt([Xs], X * Acc).
%productt([X|Xs], Acc) when X == 0 -> productt([Xs], Acc).

%
% Combining list elements: the maximum of a list
%

% Direct recursion
listmaxd([]) -> 0;
listmaxd([X]) -> X;
listmaxd([X|Xs]) -> (max(X, listmaxd(Xs))).

% Tail recursion
listmaxt([]) -> 0;
listmaxt(Xs) -> listmaxt(Xs, 0).

listmaxt([X], Acc) -> max(Acc, X);
listmaxt([X|Xs], Acc) -> listmaxt(Xs, max(Acc, X)).
