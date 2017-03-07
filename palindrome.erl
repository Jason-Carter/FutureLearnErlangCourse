-module(palindrome).
-export([palindrome/1, clean/1, clean/2, palindrome_test/0]).


% palindrome("Madam I\'m Adam") = true

palindrome([]) -> [];
palindrome(List) ->
    CleanList = clean(List),
    ReversedList = lists:reverse(CleanList),
    CleanList == ReversedList.

clean(List) -> clean(List, []).

clean([], Cleaned) -> Cleaned;
clean([X|Xs], Cleaned) when X >= $A, X =< $Z -> clean(Xs, [ X + 32 | Cleaned ]);
clean([X|Xs], Cleaned) when X >= $a, X =< $z -> clean(Xs, [ X | Cleaned ]);
clean([_X|Xs], Cleaned) -> clean(Xs, Cleaned).

palindrome_test() ->
    true = palindrome("Madam I\'m Adam"),
    true = palindrome("A man, a plan, a canal, Panama!"),
    true = palindrome("Was it a car or a cat I saw?"),
    false = palindrome("The quick brown fox jumps over the lazy dog."),
    palindrome_test_passed.
