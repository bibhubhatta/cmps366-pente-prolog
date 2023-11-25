:- begin_tests(position).

:- use_module(position).

test(position_to_string) :-
    position_to_string(0, 0, 19, PosString),
    assertion(PosString == 'A19'),
    position_to_string(18,18,19, PosString2),
    assertion(PosString2 == 'S1').

test(string_to_position) :-
    string_to_position('A19', 19, RowIndex, ColIndex),
    assertion(RowIndex == 0),
    assertion(ColIndex == 0),
    string_to_position('S1', 19, RowIndex2, ColIndex2),
    assertion(RowIndex2 == 18),
    assertion(ColIndex2 == 18).

test(position_to_string_to_position) :-
    position_to_string(0, 0, 19, PosString),
    string_to_position(PosString, 19, RowIndex, ColIndex),
    assertion(RowIndex == 0),
    assertion(ColIndex == 0),
    position_to_string(18,18,19, PosString2),
    string_to_position(PosString2, 19, RowIndex2, ColIndex2),
    assertion(RowIndex2 == 18),
    assertion(ColIndex2 == 18).

test(position_to_string_default_num_rows) :-
    position_to_string(0, 0, PosString),
    assertion(PosString == 'A19'),
    position_to_string(18,18, PosString2),
    assertion(PosString2 == 'S1').

test(string_to_position_default_num_rows) :-
    string_to_position('A19', RowIndex, ColIndex),
    assertion(RowIndex == 0),
    assertion(ColIndex == 0),
    string_to_position('S1', RowIndex2, ColIndex2),
    assertion(RowIndex2 == 18),
    assertion(ColIndex2 == 18).


test(up_position) :-
    up_position('B2', UpPosition),
    assertion(UpPosition == 'B3'),
    up_position('F6', UpPosition2),
    assertion(UpPosition2 == 'F7').

test(down_position) :-
    down_position('B2', DownPosition),
    assertion(DownPosition == 'B1'),
    down_position('F6', DownPosition2),
    assertion(DownPosition2 == 'F5').

test(left_position) :-
    left_position('B2', LeftPosition),
    assertion(LeftPosition == 'A2'),
    left_position('F6', LeftPosition2),
    assertion(LeftPosition2 == 'E6').

test(right_position) :-
    right_position('B2', RightPosition),
    assertion(RightPosition == 'C2'),
    right_position('F6', RightPosition2),
    assertion(RightPosition2 == 'G6').

test(up_left_position) :-
    up_left_position('B2', UpLeftPosition),
    assertion(UpLeftPosition == 'A3'),
    up_left_position('F6', UpLeftPosition2),
    assertion(UpLeftPosition2 == 'E7'),
    % Test that it doesn't go out of bounds
    \+ up_left_position('A19', _).

test(up_right_position) :-
    up_right_position('B2', UpRightPosition),
    assertion(UpRightPosition == 'C3'),
    up_right_position('F6', UpRightPosition2),
    assertion(UpRightPosition2 == 'G7'),
    % Test that it doesn't go out of bounds
    \+ up_right_position('S1', _).

test(down_left_position) :-
    down_left_position('B2', DownLeftPosition),
    assertion(DownLeftPosition == 'A1'),
    down_left_position('F6', DownLeftPosition2),
    assertion(DownLeftPosition2 == 'E5'),
    % Test that it doesn't go out of bounds
    \+ down_left_position('A1', _).

test(down_right_position) :-
    down_right_position('B2', DownRightPosition),
    assertion(DownRightPosition == 'C1'),
    down_right_position('F6', DownRightPosition2),
    assertion(DownRightPosition2 == 'G5'),
    % Test that it doesn't go out of bounds
    \+ down_right_position('S1', _).


:- end_tests(position).

:- run_tests(position).