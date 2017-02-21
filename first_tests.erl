-module(first_tests). 
-include_lib("eunit/include/eunit.hrl").
-export([square_test/0, square_test2/0]).

square_test() -> 
    % problem with following is that it returns the
    % result of the last equation.
    1 = first:square(1), 
    4 = first:square(2), 
    9 = first:square(3).

square_test2() -> 
    % apparently these are using macros.
    % returns ok if all succeeds, otherwise
    % fails with expected and actual values - nice
    ?assertEqual(1, first:square(1)), 
    ?assertEqual(4, first:square(2)), 
    ?assertEqual(9, first:square(3)).