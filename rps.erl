-module(rps).
-export([result/2]).
-export([tournament/2]).

% Win and Lose
beat(rock) -> paper;
beat(paper) -> scissors;
beat(scissors) -> rock.

lose(rock) -> scissors;
lose(paper) -> rock;
lose(scissors) -> paper.

% A round
result(Match, Match) -> draw;
result(Play1, Play2) ->
    case lose(Play1) == Play2 of
        true -> win;
        false -> lose
    end.

% A tournament
%    > 0 Win for left
%    0 draw
%    < 0 Win for right
%
% e.g.
%    rps:tournament([rock,rock,paper,paper],[rock,paper,scissors,rock] = -1

tournament(Left, Right) -> lists:foldr(fun(X,Y) -> X+Y end, 0,
                                       lists:map(fun resultpoint/1, 
                                                 lists:zipwith(fun result/2, Left, Right))).

resultpoint(Result) ->
    % converts a win, lose or draw to an integer representation
    case Result of
        win -> 1;
        draw -> 0;
        lose -> -1
    end.

