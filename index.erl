-module(index).
-export([get_file_contents/1,show_file_contents/1]).
-export([wordindex/1, test/1]).
-export([flattenfilecontents/1]).

%
% Indexing a file
%  Given a text file, return a list of words and the ranges of lines on which it occurs
%

%-spec wordindex(string()) -> [{string(), [{integer(),integer()}]}].

%wordindex([]) -> {[], {0, 0}};
wordindex(Name) -> flattenfilecontents(get_file_contents(Name)).
    % WordList = concat(get_file_contents(Name)),
    % {WordList, {0, 0}}.

% index:test(["jason is very busy jason", "jason busy", [], "busy busy"]).
%test(List) -> flattenfilecontents(List).
test(Lines) -> addlinenums(Lines).

% Add line numbers to each entry - have to do this first before we break the lines up so we know what the line numbers are
%
% ["jason is very busy jason", "jason busy", [], "busy busy"]
%
% becomes
%
% [{"busy busy",4},
%  {"jason busy",2},
%  {"jason is very busy jason",1}]

addlinenums(Lines) -> lines2words(addlinenums(Lines, [], 1)).

addlinenums([], NumberedLines, _LineNum) -> NumberedLines;
addlinenums([ [] | RemainingLines], NumberedLines, LineNum) -> addlinenums(RemainingLines, NumberedLines, LineNum + 1); % Removes blank paragraph lines
addlinenums([FirstLine | RemainingLines], NumberedLines, LineNum) -> addlinenums(RemainingLines, [ {FirstLine,LineNum} | NumberedLines ], LineNum + 1).

% Convert the line array into seperate words, retaining line number
lines2words(Lines) -> lines2words(Lines, []).

lines2words([], NumberedWords) -> NumberedWords;
lines2words( [ {LineOfWords, LineNum} | RemainingWords] , NumberedWords) -> lines2words(RemainingWords, [ chars2words(LineOfWords, LineNum) | NumberedWords ]).




flattenfilecontents(Lines) -> processlines(Lines, [], 1).

% Converts each line at a time to desired format
processlines([], Result, _LineNum) -> Result;
processlines([LineH | LineT], Result, LineNum) -> processlines(LineT,  join(Result, processwords(LineH, [], LineNum)), LineNum + 1).

% Given a line, convert characters to a 'word'
processwords([], Result, _) -> Result;
processwords(Line, Result, LineNum) -> chars2words(Line, [], Result, LineNum).

% Identify a 'word' from the stream of characters
chars2words (LineOfWords, LineNum) -> chars2words(LineOfWords, LineNum, [], []).

chars2words([], _LineNum, [], ArrayOfWords) -> ArrayOfWords;
chars2words([], LineNum, WordBuffer, ArrayOfWords) -> [ addword(WordBuffer, ArrayOfWords, LineNum) | ArrayOfWords ]; 
chars2words([ StringH | StringT ], LineNum, WordBuffer, ArrayOfWords) ->
    case member(StringH, " .,\n") of
        true  -> chars2words(StringT, [], [ addword(WordBuffer, ArrayOfWords, LineNum) | ArrayOfWords ], LineNum);
        false -> chars2words(StringT, join(WordBuffer, [StringH]), ArrayOfWords, LineNum)
    end.

% Add an element if it's not already present
% addword(Word, [], LineNum) -> {Word, [{LineNum,LineNum}]};
% addword(Word, [ {Word, _} | _ListTail], _LineNum) -> [];
% addword(Word, [_ | ListT], LineNum) -> addword(Word, ListT, LineNum).
addword(Word, [], LineNum) -> {Word, LineNum};
addword(Word, [_ | ListT], LineNum) -> addword(Word, ListT, LineNum).


%
% Helper functions from earlier modules
%

% Testing membership
-spec member(T, [T]) -> boolean().

member(_, []) -> false;
member(Elem, [Elem | _ListT]) -> true;
member(Elem, [_ListH | ListT]) -> member(Elem, ListT).


% My implementation of lists:concat from an earlier module
-spec concat([T]) -> [T].
concat(List) -> concat(List, []).

concat([], Result)  -> Result;
concat([ListH | ListT], Result) -> concat(ListT, join(Result, ListH)).

% My implementation of ++ called join/2 from and ealier module
% (which uses shunt also from previous modules)

-spec join([T], [T]) -> [T].
join([],[]) -> [];
join(X, []) -> X;
join([], Y) -> Y;
join(X, Y) -> shunt(shunt(X, []), Y).

shunt([],Ys)     -> Ys;
shunt([X|Xs],Ys) -> shunt(Xs,[X|Ys]).



% Used to read a file into a list of lines.
% Example files available in:
%   gettysburg-address.txt (short)
%   dickens-christmas.txt  (long)
  

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

% Show the contents of a list of strings.
% Can be used to check the results of calling get_file_contents.

show_file_contents([L|Ls]) ->
    io:format("~s~n",[L]),
    show_file_contents(Ls);
 show_file_contents([]) ->
    ok.

