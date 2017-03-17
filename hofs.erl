-module(hofs).
-export([doubleAll/1]).
-export([evens/1]).
-export([product/1]).
-export([zip/2]).
-export([zip2/2]).
-export([zip_with/3]).
-export([zip_with2/3]).

% Using higher-order functions
% Define the functions doubleAll, evens, and product using the higher-order
% functions lists:map, lists:filter and lists:foldr.

% doubleAll([]) -> [];
% doubleAll([X|Xs]) ->
%     [ 2*X | doubleAll(Xs) ].

% evens([]) -> [];
% evens([X|Xs]) when X rem 2 == 0 ->
%     [X | evens(Xs) ];
% evens([_|Xs]) ->
%     evens(Xs).

% product([]) -> 1;
% product([X|Xs]) -> X * product(Xs).

doubleAll(Xs) -> lists:map(fun double/1, Xs).
double(X) -> X*2.


evens(Xs) -> lists:filter(fun is_even/1, Xs).
is_even(X) -> X rem 2 ==0.

product(Xs) -> lists:foldr(fun mult/2, 1, Xs).
mult(X,Y) -> X*Y.

% Zipping

% a) Define a function zip/2 that “zips together” pairs of elements from
%    two lists like this:

%    zip([1,3,5,7], [2,4]) = [ {1,2}, {3,4} ]

%    where you can see that the elements from the longer list are lost.

zip([], _) -> [];
zip(_, []) -> [];
zip([X|Xs], [Y|Ys]) -> [ {X,Y} | zip(Xs,Ys) ].

% b) Define a function zip_with/3 that “zips together” pairs of elements
%    from two lists using the function in the first argument, like this:

%    zip_with(fun(X,Y) -> X+Y end, [1,3,5,7], [2,4]) = [ 3, 7 ]

zip_with(_F, [], _) -> [];
zip_with(_F, _, []) -> [];
zip_with(F, [X|Xs], [Y|Ys]) -> [ F(X,Y) | zip_with(F, Xs, Ys)].

% c) Re-define the function zip_with/3 using zip and lists:map.

zip_with2(F, Xs, Ys) -> lists:map(fun({X,Y}) -> F(X,Y) end, zip(Xs, Ys)).

% d) Re-define zip/2 using zip_with/3.

zip2(Xs,Ys) -> zip_with2(fun(X,Y) -> {X,Y} end, Xs, Ys).
