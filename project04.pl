% project04.pl
% CSE4250 Programming Language Concepts
% Matteo Caruso, Emma Bahr, Joshua Cajuste
% Project 04

% Main entry point
main :-
    read_line_to_string(user_input, NStr),
    number_string(N, NStr),
    N1 is N - 1,
    numlist(0, N1, Indices),
    maplist(index_to_letter, Indices, Vars),
    read_line_to_string(user_input, ValuesLine),
    split_string(ValuesLine, " ", "", ValuesStr),
    maplist(value_bool, ValuesStr, Values),
    pairs_keys_values(VarMap, Vars, Values),
    read_line_to_string(user_input, ExprLine),
    split_string(ExprLine, " ", "", ExprTokens),
    evaluate(ExprTokens, VarMap, [], FinalStack),
    FinalStack = [Result],  % explicitly match
    (Result == true -> write('T') ; write('F')).

% Map 'T' or 'F' to Prolog booleans
value_bool("T", true).
value_bool("F", false).

% Map 0 -> 'A', 1 -> 'B', etc.
index_to_letter(I, Letter) :-
    Code is 65 + I,
    char_code(Letter, Code).

% Evaluate the postfix boolean expression
evaluate([], _, [Result], [Result]).
evaluate([Token|Rest], VarMap, Stack, FinalStack) :-
    ( member(Token-Value, VarMap) ->
        evaluate(Rest, VarMap, [Value|Stack], FinalStack)
    ; Token = "*" ->
        [Right, Left | StackRest] = Stack,
        (Left == true, Right == true -> Res = true ; Res = false),
        evaluate(Rest, VarMap, [Res|StackRest], FinalStack)
    ; Token = "+" ->
        [Right, Left | StackRest] = Stack,
        (Left == true ; Right == true -> Res = true ; Res = false),
        evaluate(Rest, VarMap, [Res|StackRest], FinalStack)
    ; Token = "-" ->
        [Val | StackRest] = Stack,
        (Val == true -> Res = false ; Res = true),
        evaluate(Rest, VarMap, [Res|StackRest], FinalStack)
    ).
