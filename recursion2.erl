-module(recursion2).
-export([fib/1, fib/3, fib_tests/0, perfect_tests/0, perfect/1]).

% An efficient implementation of fibonacci

fib(N) -> fib(N, 0, 1).

fib(0, Prev, _) -> Prev;
fib(Cnt, Prev, Curr) -> fib(Cnt-1, Curr, Prev+Curr).

% fib(4)
% fib(4,0,1)
% fib(3,1,1)
% fib(2,1,2)
% fib(1,2,3)
% fib(0,3,5)
% 5

fib_tests() ->
    0 = fib(0),
    1 = fib(1),
    1 = fib(2),
    2 = fib(3),
    3 = fib(4),
    5 = fib(5),
    8 = fib(6),
    fib_tests_passed.   

% A positive integer is perfect when it is the sum of its divisors,
% e.g. 6=1+2+3, 28=1+2+4+7+14.
%
% Take a positive number N and returns a boolean which indicates whether
% or not the number is perfect. 

perfect_tests() ->
    true = perfect(6),
    true = perfect(28),
    true = perfect(496),
    true = perfect(8128),
    false = perfect(1),
    false = perfect(2),
    false = perfect(3),
    false = perfect(4),
    false = perfect(5),
    perfect_tests_passed.

perfect(N) -> perfect(N, N-1, 0).

perfect(N, 0, Acc) -> N == Acc;
perfect(N, Cnt, Acc) when (N rem Cnt) > 0 -> perfect(N, Cnt-1, Acc);
perfect(N, Cnt, Acc) when (N rem Cnt) == 0 -> perfect(N, Cnt-1, Acc + Cnt).

