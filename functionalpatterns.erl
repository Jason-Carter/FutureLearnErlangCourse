-module(funcionalpatterns).
-export([all_areas/1]).
-export([map/2]).

%
% Note this is full of errors as it's notes and snippets taken from the course
%


%
% map
%

% e.g. Finding the area of all elements ... the only specific thing is applying area
all_areas([])       -> [];
all_areas([X|Xs])   -> [ area(X) | all_areas(Xs)].

% Apply F instead of area ... and add F as an argument
map(F,[])       -> [];
map(F,[X|Xs])   -> [ F(X) | map(F, Xs)].

% Here's the final definition:
all_areas(Shapes) -> map(fun area/1, Shapes).

% Built into the standard library, lists:map

%
% filter
%

% e.g. find the circles in a list of Shapes
circles([]) -> [];

circles( [{circle, {X,Y}, R} | Xs]) ->
    [ {circle, {X,Y}, R} | circles(Xs) ];

circles( [{rectangle, {_,_}, _,_} | Xs]) ->
circles(Xs).

% generalise the pattern...
filter(P, []) -> [];

filter(P, [X|Xs]) ->
    case P(X) of
        true  -> [X | filter(P, Xs)];
        false -> filter(P, Xs)
    end.

% and you can use filter as follows

circles(Shapes) -> filter(fun is_circle/1, Shapes).

is_circle( {circle, {_,_}, _})      -> true;
is_circle( {rectanble, {_,_}, _,_}) -> false.

% Built into standard library as lists:filter

%
% reduce
%

% e.g. sum the values of a lists

sum([])     -> 0;
sum([X|Xs]) -> X+ sum(Xs).

% generalise the pattern:
reduce(Combine, Start, []) -> Start;

reduce(Combine, Start, [X|Xs]) ->
    Combine(X, reduce(Combine, Start, Xs)).

% and you can use sum as follows:
sum(Xs) -> reduce(fun plus/2, 0, Xs).

plus(X,Y) -> X+Y.

% Built into the standard library as lists:foldr
