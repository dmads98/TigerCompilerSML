Dhanush Madabusi (dm322)
Prathikshaa Rangarajan (pr109)


To run Lexer:
CM.Make "sources.cm";
Parse.parse "/path/to/.tig";

COMMENTS:
* When we see the start of a comment, i.e. /* when in the INITIAL state, we start the state COMMENT and increment the nestedCom variable which keeps track of the level of nesting of the comments.
* When we see the start of a comment while in COMMENT state, we simply increment the nestedCom variable.
* When we see the end of a comment, i.e. the */ when in the COMMENT state, we decrement the level of nesting, i.e. nestedCom. Also, before decrementing, if we see the the value of nestedCom has dropped to 1, we reset to the INITIAL state.


STRINGS:
* When a string start token " is seen in the INITIAL state, we switch to the STR state and set strStart to contain the start of the string.
* In the STR state, the allowed escape sequences are translated to their original meanings and saved in the string. This includes the usual formatting \n, \t, \^c for control characters, \ddd for ASCII code characters, \" and \\.
* The feature that allows long strings to span across different lines starts a new state, FORMATSEQ, if already in the STR state. This is written after all the rules to match the escape sequences. Therefore, by rule precedence, we will only enter this state if the \ is followed by something other than the allowed escape sequences.
* When in the FORMATSEQ state, if we see any characters apart from the allowed space, tab, new line or form feed, this throws an error and continues.
* The FORMATSEQ state ends when we see another \ and returns to STR state.
* When the STR state ends on seeing an unescaped quote " again, we reset strStart to ~1.

ERROR HANDLING:
Whenever an invalid character is encountered, we display an error message with the position and continue.

EOF HANDLING:
* When we reach the end of the file is reached, if there are still any open comments or strings, which we can verify using the nestedCom and strStart values, we throw an appropriate error.
