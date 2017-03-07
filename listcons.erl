-module(listcons).
-export([double/1]).
-export([doubleelems/1]).
-export([even/1,evens/1]).

% Erlang Course activity 2.9

% Transforming List Elements
% Define an Erlang function double/1 to double the elements of a list of numbers.

double(X)   -> X*2.

doubleelems([])     -> [];
doubleelems([X|Xs]) -> [ double(X) | doubleelems(Xs) ].

% Filtering lists
% Define a function evens/1 that extracts the even numbers from a list of integers.

even(X) -> (X rem 2) == 0.

evens([])       -> [];
evens([X|Xs])   -> 
    case even(X) of
        true -> [ X | evens(Xs)];
        _ -> evens(Xs)
    end.

