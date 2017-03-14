-module(index).
-export([get_file_contents/1,show_file_contents/1]).
-export([wordindex/1, test/1]).

%
% Indexing a file
%  Given a text file, return a list of words and the ranges of lines on which it occurs
%

%-spec wordindex(string()) -> [{string(), [{integer(),integer()}]}].

%wordindex([]) -> {[], {0, 0}};
wordindex(Name) -> lines2words(addlinenums(get_file_contents(Name))).
    % WordList = concat(get_file_contents(Name)),
    % {WordList, {0, 0}}.

% index:test(["jason is very busy jason", "jason busy", [], "busy busy", "very"]).
test(Lines) -> lines2words(addlinenums(Lines)).

% Add line numbers to each entry - have to do this first before we break the lines up so we know what the line numbers are
%
% ["jason is very busy jason", "jason busy", [], "busy busy", "very"]
%
% becomes
%
% [{"jason is very busy jason",1},
%  {"jason busy",2},
%  {"busy busy",4},
%  {"very",5}]

addlinenums(Lines) -> addlinenums(Lines, [], 1).

addlinenums([], NumberedLines, _LineNum) -> NumberedLines;
addlinenums([ [] | RemainingLines], NumberedLines, LineNum) -> addlinenums(RemainingLines, NumberedLines, LineNum + 1); % Removes blank paragraph lines
addlinenums([FirstLine | RemainingLines], NumberedLines, LineNum) -> addlinenums(RemainingLines, join(NumberedLines, [{FirstLine,LineNum}]), LineNum + 1).

% Convert the line array into seperate words, retaining line numbers
%
% [{"jason is very busy jason",1},
%  {"jason busy",2},
%  {"busy busy",4},
%  {"very",5}]

%
% becomes
%
% [{"jason",1},
%  {"is",1},
%  {"very",1},
%  {"busy",1},
%  {"jason",1},
%  {"jason",2},
%  {"busy",2},
%  {"busy",4},
%  {"busy",4},
%  {"very",5}]

lines2words(Lines) -> lines2words(Lines, []).

lines2words([], NumberedWords) -> NumberedWords;
lines2words( [ {LineOfChars, LineNum} | RemainingWords] , NumberedWords) -> lines2words(RemainingWords, join(NumberedWords, numberthewords(chars2words(LineOfChars), LineNum))).

numberthewords([], _) -> [];
numberthewords(Words, LineNum) -> numberthewords(Words, [], LineNum).

numberthewords([], NumberedWords, _) -> NumberedWords;
numberthewords([FirstWord | RemainingWords ], NumberedWords, LineNum) -> numberthewords(RemainingWords, [ {FirstWord, LineNum} | NumberedWords], LineNum).

% Identify a 'word' from the stream of characters
chars2words (LineOfWords) -> chars2words(LineOfWords, [], []).

chars2words([], [], ArrayOfWords) -> ArrayOfWords;
chars2words([], WordBuffer, ArrayOfWords) -> join([WordBuffer], ArrayOfWords); 
chars2words([ FirstChar | RemainingChars ], WordBuffer, ArrayOfWords) ->
    case member(FirstChar, " .,\n") of
        true  -> chars2words(RemainingChars, [], addwordnodupe(WordBuffer, ArrayOfWords));
        false -> chars2words(RemainingChars, join(WordBuffer, [FirstChar]), ArrayOfWords)
    end.

% TODO: Why is this not removing existing members?
addwordnodupe(Word, WordArray) ->
    case member(Word, WordArray) of
        true -> WordArray;
        false -> join([Word], WordArray)
    end.

% Roll up the lines numbers so each word is associated with a list of line numbers
%
% [{"jason",1},
%  {"is",1},
%  {"very",1},
%  {"busy",1},
%  {"jason",1},
%  {"jason",2},
%  {"busy",2},
%  {"busy",4},
%  {"busy",4},
%  {"very",5}]
%
% becomes
%
% ...

indexwords([]) -> [];
indexwords(NumberedWords) -> indexwords(NumberedWords, []).

indexwords([], IndexedWords) -> IndexedWords;
indexwords([ {FirstNumWord, LineNum} | RemainingNumWords], IndexedWords) -> indexwords(RemainingNumWords, updatewordindex({FirstNumWord, LineNum}, IndexedWords)).


updatewordindex({FirstNumWord, LineNum}, []) -> [{FirstNumWord, [LineNum]}];
updatewordindex({FirstNumWord, LineNum}, IndexedWords) -> updatewordindex({FirstNumWord, LineNum}, IndexedWords, []).

updatewordindex({FirstNumWord, LineNum}, [], IndexedWordsResult) ->
    % Nothing matched, so add to results as-is because it's the first entry for that word
    join(IndexedWordsResult, [{FirstNumWord, [LineNum]}]);
updatewordindex({MatchedWord, LineNum}, [ {MatchedWord, LineNums} | RemainingIndexedWords ], IndexedWordsResult) ->
    % Matches existing entry, so update that entry and add to IndexedWordsResult
    updatewordindex({MatchedWord, LineNum}, RemainingIndexedWords, join(IndexedWordsResult, [{MatchedWord, join(LineNums, [LineNum] )}]));
updatewordindex({FirstNumWord, LineNum}, [ {UnMatchedWord, LineNums} | RemainingIndexedWords ], IndexedWordsResult) ->
    % Nothing matched, add as-is to IndexedWordsResult and try the remaining words
    updatewordindex({FirstNumWord, LineNum}, RemainingIndexedWords, join(IndexedWordsResult, [{UnMatchedWord, LineNums}])).


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

