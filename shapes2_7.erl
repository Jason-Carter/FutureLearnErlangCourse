-module(shapes2_7).
-export([circles/1]).
-export([sum/1,sum/2,all_areas/1, total_area/1]).

circles([])     -> [] ; 

% The following definition:
%
% circles({circle,{X,Y},R}) -> [ {circle,{X,Y},R} | circles(Xs) ];
% circles({rectangle,{_,_},_,_}) -> circles(Xs).
%
% Can be written using the nicer format:
%

circles( [X | Xs] ) ->
    case X of
		% The C alias is probably not needed since the circle is in X
		{circle,{_,_},_}=C -> [ C | circles(Xs) ];
		_ -> circles(Xs)
end.


% [ {circle, {1,2}, 2}, {rectangle, {5, 4}, 3, 2}, {circle, {-1, -2}, 1} ]

sum(Xs) -> sum(Xs, 0).

sum([], S)		-> S;
sum([X|Xs], S)	-> sum(Xs, X + S).

all_areas([]) -> [];
all_areas([X|Xs]) -> [ assignment1:area(X) | all_areas(Xs) ].

% total_area(Shapes) ->
% 	AllAreas = all_areas(Shapes),
% 	sum(AllAreas).
%
% or

total_area(Shapes) -> sum(all_areas(Shapes)).
