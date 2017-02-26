-module(second).
-export([hypotenuse/2, perim/2, area/2]).

hypotenuse(Side1, Side2) ->
    math:sqrt(first:square(Side1) + first:square(Side2)).

perim(Side1, Side2) ->
    % Could also have done this in one line (but liked breaking it over two):
    % Side1 + Side2 + hypotenuse(Side1, Side2).
    Side3 = hypotenuse(Side1, Side2),
    Side1 + Side2 + Side3.

area(Side1, Side2) ->
    % Should have used first:mult...
    %(Side1 * Side2) / 2.
    first:mult(Side1, Side2) / 2.