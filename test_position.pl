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
    assertion(UpLeftPosition2 == 'E7').
    % Test that it doesn't go out of bounds
    % \+ up_left_position('A19', _).

test(up_right_position) :-
    up_right_position('B2', UpRightPosition),
    assertion(UpRightPosition == 'C3'),
    up_right_position('F6', UpRightPosition2),
    assertion(UpRightPosition2 == 'G7').
    % Test that it doesn't go out of bounds
    % \+ up_right_position('S1', _).

test(down_left_position) :-
    down_left_position('B2', DownLeftPosition),
    assertion(DownLeftPosition == 'A1'),
    down_left_position('F6', DownLeftPosition2),
    assertion(DownLeftPosition2 == 'E5').
    % Test that it doesn't go out of bounds
    % \+ down_left_position('A1', _).

test(down_right_position) :-
    down_right_position('B2', DownRightPosition),
    assertion(DownRightPosition == 'C1'),
    down_right_position('F6', DownRightPosition2),
    assertion(DownRightPosition2 == 'G5').
    % Test that it doesn't go out of bounds
    % \+ down_right_position('S1', _)

test(get_neigbors) :-
    get_neighbors('B2', Neighbors),
    ExpectedNeighbors = ['A3', 'B3', 'C3', 'A2', 'C2', 'A1', 'B1', 'C1'],
    sort(ExpectedNeighbors, SortedExpectedNeighbors),
    sort(Neighbors, SortedNeighbors),
    assertion(SortedExpectedNeighbors == SortedNeighbors),
    get_neighbors('A1', Neighbors2),    
    ExpectedNeighbors2 = ['A2', 'B2', 'B1'],
    sort(ExpectedNeighbors2, SortedExpectedNeighbors2),
    sort(Neighbors2, SortedNeighbors2),
    assertion(SortedExpectedNeighbors2 == SortedNeighbors2),
    get_neighbors('S1', Neighbors3),
    ExpectedNeighbors3 = ['R2', 'S2', 'R1'],
    sort(ExpectedNeighbors3, SortedExpectedNeighbors3),
    sort(Neighbors3, SortedNeighbors3),
    assertion(SortedExpectedNeighbors3 == SortedNeighbors3),
    get_neighbors('S19', Neighbors4),
    ExpectedNeighbors4 = ['R19', 'S18', 'R18'],
    sort(ExpectedNeighbors4, SortedExpectedNeighbors4),
    sort(Neighbors4, SortedNeighbors4),
    assertion(SortedExpectedNeighbors4 == SortedNeighbors4),
    get_neighbors('A19', Neighbors5),
    ExpectedNeighbors5 = ['A18', 'B18', 'B19'],
    sort(ExpectedNeighbors5, SortedExpectedNeighbors5),
    sort(Neighbors5, SortedNeighbors5),
    assertion(SortedExpectedNeighbors5 == SortedNeighbors5),
    get_neighbors('S10', Neighbors6),
    ExpectedNeighbors6 = ['R10', 'S9', 'S11', 'R9', 'R11'],
    sort(ExpectedNeighbors6, SortedExpectedNeighbors6),
    sort(Neighbors6, SortedNeighbors6),
    assertion(SortedExpectedNeighbors6 == SortedNeighbors6).
    

:- end_tests(position).

:- run_tests(position).