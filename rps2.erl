-module(rps2).
-export([play/1,echo/1,play_two/3,rock/1,no_repeat/1,const/1,enum/1,cycle/1,rand/1,val/1,tournament/2]).


%
% play one strategy against another, for N moves.
%

play_two(StrategyL,StrategyR,N) ->
    play_two(StrategyL,StrategyR,[],[],N).

% tail recursive loop for play_two/3
% 0 case computes the result of the tournament

% FOR YOU TO DEFINE
% REPLACE THE dummy DEFINITIONS

play_two(_,_,PlaysL,PlaysR,0) ->
   dummy;

play_two(StrategyL,StrategyR,PlaysL,PlaysR,N) ->
   dummy.

%
% interactively play against a strategy, provided as argument.
%

play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy,[]).

% tail recursive loop for play/1

play(Strategy,Moves) ->
    {ok,P} = io:read("Play: "),
    Play = expand(P),
    case Play of
	stop ->
	    io:format("Stopped~n");
	_    ->
	    Result = result(Play,Strategy(Moves)),
	    io:format("Result: ~p~n",[Result]),
	    play(Strategy,[Play|Moves])
    end.

%
% auxiliary functions
%

% transform shorthand atoms to expanded form
    
expand(r) -> rock;
expand(p) -> paper;		    
expand(s) -> scissors;
expand(X) -> X.

% result of one set of plays

result(rock,rock) -> draw;
result(rock,paper) -> lose;
result(rock,scissors) -> win;
result(paper,rock) -> win;
result(paper,paper) -> draw;
result(paper,scissors) -> lose;
result(scissors,rock) -> lose;
result(scissors,paper) -> win;
result(scissors,scissors) -> draw.

% result of a tournament

tournament(PlaysL,PlaysR) ->
    lists:sum(
      lists:map(fun outcome/1,
		lists:zipwith(fun result/2,PlaysL,PlaysR))).

outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

% transform 0, 1, 2 to rock, paper, scissors and vice versa.

enum(0) -> rock;
enum(1) -> paper;
enum(2) -> scissors.

val(rock)     -> 0;
val(paper)    -> 1;
val(scissors) -> 2.

% give the play which the argument beats.

beats(rock)     -> scissors;
beats(paper)    -> rock;
beats(scissors) -> paper.

loses(rock)     -> paper;
loses(paper)    -> scissors;
loses(scissors) -> rock.

%
% strategies.
%
echo([])       -> paper;
echo([Last|_]) -> Last.

rock(_) -> rock.

no_repeat([]) -> paper;
no_repeat([Previous|_]) -> beats(Previous).

% Not quite sure what this one is for... so assuming 'constant'
% which just returns one particular value.
const(_Play) -> paper.

% I peeked at other solutions for this one as couldn't figure out
% how to cycle through the values, and I liked this solution
cycle(Xs) -> enum(length(Xs) rem 3).

% Returns a random rock paper or scissors
rand(_) -> enum(rand:uniform(3) - 1).

% Choose the least frequent play, assuming that in the long run your opponent will
% play each choice equally
leastfrequent([])    -> rand([]);
leastfrequent(Plays) -> mincount(addcounts(Plays)).

% Choose the most frequent play, assuming that in the long run your opponent is going to
% play that choice more often than the others.
mostfrequent([])    -> rand([]);
mostfrequent(Plays) -> maxcount(addcounts(Plays)).


mincount([{rock, CntRock},{paper, CntPaper},{scissors, CntScissors}]) ->
    % Returns the play with the least number of uses
    matchcount(min(min(CntRock, CntPaper), min(CntPaper, CntScissors)), CntRock, CntPaper, CntScissors).

maxcount([{rock, CntRock},{paper, CntPaper},{scissors, CntScissors}]) ->
    % Returns the play with the most number of uses
    matchcount(max(max(CntRock, CntPaper), max(CntPaper, CntScissors)), CntRock, CntPaper, CntScissors).

% Doesnt' matter if the count matches >1, the first hit will always be used
matchcount(MatchCount, MatchCount, _PaperCount, _ScissorsCount) -> rock;
matchcount(MatchCount, _RockCount, MatchCount, _ScissorsCount)  -> paper;
matchcount(MatchCount, _Rockcount, _PaperCount, MatchCount)     -> scissors.

addcounts(Plays) -> addcounts(Plays, [{rock, 0},{paper, 0},{scissors, 0}]).

addcounts([], Counts) -> Counts;
addcounts([rock | Plays] , [{rock, CntRock},{paper, CntPaper},{scissors, CntScissors}])     -> addcounts(Plays , [{rock, CntRock + 1},{paper, CntPaper},{scissors, CntScissors}]);
addcounts([paper | Plays] , [{rock, CntRock},{paper, CntPaper},{scissors, CntScissors}])    -> addcounts(Plays , [{rock, CntRock},{paper, CntPaper + 1},{scissors, CntScissors}]);
addcounts([scissors | Plays] , [{rock, CntRock},{paper, CntPaper},{scissors, CntScissors}]) -> addcounts(Plays , [{rock, CntRock},{paper, CntPaper},{scissors, CntScissors + 1}]).


% Todo: Take a list of strategies and each play chooses a random one to apply.

% Todo: Take a list of strategies and each play chooses from the list the
%       strategy which gets the best result when played against the list
%       of plays made so far
%
% Will have to play each strategy, store the results, then return the best
