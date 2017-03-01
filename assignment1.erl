-module(assignment1).
-export([area/1, perimeter/1]).
-export([enclose/1, test_enclose/0]).
-export([bits/1, test_bits/0]).
-export([dbits/1, tbits/1, tbits/2]).

%
% Shapes
%

% Note the shape definitions are taken from module 1.23 and
% include the X, Y center postion.

area({circle, {_X, _Y}, R}) -> math:pi()*R*R;
area({rectangle, {_X, _Y}, H, W}) -> H*W;
area({triangle, {_X, _Y}, A, B, C}) -> 
    % Uses Heron's Formula to calculate any triangle's area
    S = (A + B + C) / 2,    % Could have called perimeter function below, passing the triangle for A+B+C, but this is easier to read
    math:sqrt(S * (S-A) * (S-B) * (S-C)).

perimeter({circle, {_X, _Y}, R}) -> 2 * math:pi() * R;
perimeter({rectangle, {_X, _Y}, H, W}) -> 2 * (H + W);
perimeter({triangle, {_X, _Y}, A, B, C}) -> A + B + C.

enclose({circle, {X, Y}, R}) ->
    % A circle can be enclosed by a rectangle whose height and
    % width are equal to the circle's diameter
    D = 2 * R,
    {rectangle, {X, Y}, D, D};
enclose({triangle, {X, Y}, A, B, C}) ->
    % A triangle can be enclosed be a rectangle whose height and
    % width are equal to the two longest sides of the triangle
    H = max(A, max(B, C)),
    W = min(max(A,B), max(A,C)),
    {rectangle, {X, Y}, H, W}.

% Test cases, should return the atom test_enclose_passed if enclose({}) works as expected
test_enclose() ->
    {rectangle,{0,0},10,10}    = enclose({circle, {0, 0}, 5}),
    {rectangle,{0,0},9.0,9.0}  = enclose({circle, {0, 0}, 4.5}),
    {rectangle,{0,0},5,4}      = enclose({triangle, {0, 0}, 3, 4, 5}),
    {rectangle,{0,0},12.3,7.7} = enclose({triangle, {0, 0}, 5.5, 12.3, 7.7}),
    test_enclose_passed.

%
% Summing the bits
%

% Take a positive integer N and returns the sum of the bits in the binary representation.
% For example bits(7) is 3 (0111) and bits(8) is 1 (1000).

% Notes and Comments
%
% The conversion to binary using integer_to_list was the result of a google search, so I
% did that quite quickly. I was stuck for a while wondering what to do next, before realising
% that a string was actually a list and I could simply process the Head and recursively
% pass the Tail (although I couldn't remember if we've covered that on this course). That's
% when I hit problems with string and integer conversions and fell down the rabbit hole...
%
% This took me quite a bit of time to resolve (2 - 3 days!) as I was stuck trying to convert
% the string array of the bits to numbers (integers), and found some interesting erlang
% such as:
%
%    Indexstr = "4", {I, _} = string:to_integer(Indexstr), I.
%
% before realising I could pattern match and simply increment Sum if the Head of the
% list was "1", or 49 for the numeric equivalent. Once I clicked for the pattern matching
% everything else fell into place.

countbits([], Sum) -> Sum;
countbits([49|T], Sum) -> countbits(T, Sum + 1); % 49 = "1", so add 1 to Sum
countbits([_|T], Sum) -> countbits(T, Sum). % Don't sum anything else (looking at you "0")

countbits(X) -> countbits(X,0). % Just calls the main countbits/2 function with an initialised counter

bits(N) -> countbits(integer_to_list(N, 2)).

% Some reduction I did during testing...
% bits(7)
% countbits(111)
% countbits(111,0)
% countbits(11,1)
% countbits(1,2)
% countbits([],3)
% 3

% Test cases, should return the atom test_bits_passed if bits(N) works as expected
test_bits() ->
    0 = bits(0),
    1 = bits(1),
    1 = bits(2),
    2 = bits(3),
    1 = bits(4),
    2 = bits(5),
    2 = bits(6),
    3 = bits(7),
    1 = bits(8),
    2 = bits(9),
    4 = bits(15),
    5 = bits(31),
    6 = bits(63),
    test_bits_passed.
    
% From Heinz's review:

%% direct recursion
dbits(0) -> 0;
dbits(N) -> (N rem 2) + dbits(N div 2).

%% tail recursive version
tbits(N) -> tbits(N,0).
tbits(0,S) -> S;
tbits(N,S) -> tbits(N div 2, (N rem 2) + S).
