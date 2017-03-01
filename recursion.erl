-module(recursion).
-export([fib/1, pieces/1, cake/2, factorial/1, binomialcoefficient/2]).

% Give a recursive definition of the function fib/1 computing the
% Fibonacci numbers...

fib(0) -> 0;
fib(1) -> 1;
fib(X) -> (fib (X-1)) + (fib (X-2)).

% and give a step-by-step evaluation of fib(4).

% fib(4)
% fib(3) + fib(2)
% fib(2) + fib(1) + fib(1) + fib(0)
% fib(1) + 1 + 1 + 0
% 1 + 1 + 1 + 0
% 3

% According to wikipedia:
%
%    https://en.wikipedia.org/wiki/Lazy_caterer%27s_sequence
%
% The proof section contains the following:
%
%    Since f(0)=1, because there is one piece before any cuts are
%    made, this can be rewritten as f(n)=1+(1+2+3+...+n).
%
% Which looks like a formula we can recurse :-)
pieces(0) -> 1;
pieces(Cuts) -> pieces(Cuts-1) + Cuts.

% Again, 3D version of this is the cake number, according to Wikipedia:
%
%   https://en.wikipedia.org/wiki/Cake_number
%
factorial(0) -> 1;
%factorial(1) -> 1;
factorial(N) when N < 0 -> 1;
factorial(N) -> factorial(N-1) * N.

binomialcoefficient(N,K) -> factorial(N) / (factorial(K) * factorial(N - K)).

cake(N,0) -> binomialcoefficient(N,0);
cake(N,K) -> cake(N, K-1) + binomialcoefficient(N,K).

% I think K is the dimension, and N is the number of cuts
% so cake(5,3) means 5 cuts in 3 dimensions (=26).
