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

% Note: could have used lists:sum rather than lists:foldr

% This would be better converted to function head pattern matching rather than a case statement
% resultpoint(Result) ->
%     % converts a win, lose or draw to an integer representation
%     case Result of
%         win -> 1;
%         draw -> 0;
%         lose -> -1
%     end.
resultpoint(win)  -> 1;
resultpoint(draw) -> 0;
resultpoint(lose) -> -1.

% transform 0, 1, 2 to rock, paper, scissors and vice versa

enum(0) -> rock;
enum(1) -> paper;
enum(2) -> scissors.

val(rock)     -> 0;
val(paper)    -> 1;
val(scissors) -> 2.
