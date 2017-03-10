-module(index).
-export([get_file_contents/1,show_file_contents/1]).
-export([wordindex/1]).

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

%
% Indexing a file
%  Given a text file, return a list of words and the ranges of lines on which it occurs
%

%-spec wordindex(string()) -> [{string(), [{integer(),integer()}]}].

%wordindex([]) -> {[], {0, 0}};
wordindex(Name) -> processlines(get_file_contents(Name), []).
    % WordList = concat(get_file_contents(Name)),
    % {WordList, {0, 0}}.

%
% Helper functions created for this exercise
%]

processlines([], Result) -> Result;
processlines([LineH | LineT], Result) -> processlines(LineT,  join(Result, processwords(LineH, []))).

processwords([], Result) -> Result;
processwords(Line, Result) -> chars2words(Line, [], Result).


 %[ {LineH, [{0}]} | processwords(LineT, Result)].

chars2words([], [], Result) -> Result;
chars2words([], WordBuffer, Result) -> [{WordBuffer, [{0,0}]} | Result];
chars2words([ StringH | StringT ], WordBuffer, Result) ->
    case member(StringH, " .,\n") of
        true  -> chars2words(StringT, [], [ {WordBuffer, [{0,0}]} | Result ]);
        false -> chars2words(StringT, join(WordBuffer, [StringH]), Result)
    end.


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