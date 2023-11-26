:- begin_tests(board).

:-use_module(board).
:-use_module(game_state).

test(get_row) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_row(Board, 4, Row4),
    assertion(Row4 ==  [ o, o, o, o, w, w, b, o, o, o, o, o, o, o, o, o, o, o, o ]),
    get_row(Board, 1, Row1),
    assertion(Row1 ==  [ o, o, o, b, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]),
    get_row(Board, 19, Row19),
    assertion(Row19 == [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]).


test(get_col) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_col(Board, d, ColD),
    assertion(ColD ==  [o, b, b, w, o, o, o, o, o, o, o, o, o, o, o, o, w, w, b]),
    get_col(Board, g, ColG),
    assertion(ColG == [o,  o,  o,  w,  o,  o,  o,  o,  o,  o,  o,  o,  o,  o,  o,  b,  o,  o,  o]).

test(get_char_list) :-
    get_char_list(3, CharList),
    assertion(CharList == ['A', 'B', 'C']),
    get_char_list(19, CharList2),
    assertion(CharList2 == ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
                            'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S']).

test(mark_col) :-
    TestBoard = [[o,o,o], [o,o,o], [o,o,o]],
    mark_columns(TestBoard, NewBoard),
    assertion(NewBoard == [[o,o,o], [o,o,o], [o,o,o], ['A', 'B', 'C']]).

test(mark_row) :-
    TestBoard = [[o,o,o], [o,o,o], [o,o,o]],
    mark_rows(TestBoard, NewBoard),
    assertion(NewBoard == [[3, o,o,o], [2, o,o,o], [1, o,o,o]]).

test(cartesian_board) :-
    TestBoard = [[o,o,o], [o,o,o], [o,o,o]],
    cartesian_board(TestBoard, NewBoard),
    assertion(NewBoard == [[3, o,o,o], [2, o,o,o], [1, o,o,o], [' ', 'A', 'B', 'C'] ]).

% test(print_board) :-
%     read_game_state('serials/4.pl', GameState),
%     get_board(GameState, Board),
%     cartesian_board(Board, NewBoard),
%     print_board(NewBoard).

test(get_stone) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_stone(Board, 'A1', StoneA1),
    assertion(StoneA1 == o),
    get_stone(Board, 'D2', StoneD2),
    assertion(StoneD2 == w),
    \+ get_stone(Board, 'T19', _),
    \+ get_stone(Board, 'A20', _),
    \+ get_stone(Board, 'T0', _),
    get_stone(Board, 'a1', Stone_a1),
    assertion(Stone_a1 == o),
    get_stone(Board, 'd2', Stone_d2),
    assertion(Stone_d2 == w).

test(get_stones) :-
    Postions = ['a1', 'D2', 'A1', 'd2', 'p5'],
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_stones(Board, Postions, Stones),
    assertion(Stones == [o, w, o, w, b]).

test(set_stone) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_stone(Board, 'A1', OldStoneA1),
    assertion(OldStoneA1 == o),
    set_stone(Board, 'A1', w, NewBoard),
    get_stone(NewBoard, 'A1', StoneA1),
    assertion(StoneA1 == w),
    get_stone(Board, 'D2', OldStoneD2),
    assertion(OldStoneD2 == w),
    set_stone(Board, 'D2', o, NewBoard2),
    get_stone(NewBoard2, 'D2', StoneD2),
    assertion(StoneD2 == o),
    \+ set_stone(Board, 'T19', o, _),
    \+ set_stone(Board, 'A20', o, _),
    \+ set_stone(Board, 'T0', o, _),
    set_stone(Board, 'a1', w, NewBoard3),
    get_stone(NewBoard3, 'a1', Stone_a1),
    assertion(Stone_a1 == w),
    set_stone(Board, 'd2', o, NewBoard4),
    get_stone(NewBoard4, 'd2', Stone_d2),
    assertion(Stone_d2 == o),
    set_stone(Board, 'p5', black, NewBoard5),
    get_stone(NewBoard5, 'p5', Stone_p5),
    assertion(Stone_p5 == b),
    set_stone(Board, 'p5', white, NewBoard6),
    get_stone(NewBoard6, 'p5', Stone_p5_2),
    assertion(Stone_p5_2 == w),
    set_stone(Board, 'p5', empty, NewBoard7),
    get_stone(NewBoard7, 'p5', Stone_p5_3),
    assertion(Stone_p5_3 == o).
    

test(convert_to_sequences) :-
      TestSequence1 = [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]
    , ExpectedSequence1 = [[o, o, o, o, o, o, o, o, o, o, o, o, o, o, o]]
    , convert_to_sequences(TestSequence1, ConvertedSequence1)
    , assertion(ConvertedSequence1 == ExpectedSequence1)
    , TestSequence2 = [o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, w, w, b]
    , ExpectedSequence2 = [[o, o, o, o, o, o, o, o, o, o, o, o, o, o, o], [w, w], [b]]
    , convert_to_sequences(TestSequence2, ConvertedSequence2)
    , assertion(ConvertedSequence2 == ExpectedSequence2)
    , TestSequence3 = [o,o,b,b,b,b,b,b,w,w,w,b,b,o,w,w]
    , ExpectedSequence3 = [[o,o], [b,b,b,b,b,b], [w,w,w], [b,b], [o], [w,w]]
    , convert_to_sequences(TestSequence3, ConvertedSequence3)
    , assertion(ConvertedSequence3 == ExpectedSequence3)
    . 

test(get_empty_positions) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_empty_positions(Board, EmptyPositions),
    assertion(EmptyPositions == ['A19', 'B19', 'C19', 'D19', 'E19', 'F19', 'G19', 'H19', 'I19', 'J19', 'K19', 'L19', 'M19', 'N19', 'O19', 'P19', 'Q19', 'R19', 'S19', 'A18', 'B18', 'C18', 'F18', 'G18', 'H18', 'I18', 'J18', 'K18', 'L18', 'M18', 'N18', 'O18', 'P18', 'Q18', 'R18', 'S18', 'A17', 'B17', 'C17', 'E17', 'G17', 'H17', 'I17', 'J17', 'K17', 'L17', 'M17', 'N17', 'O17', 'P17', 'Q17', 'R17', 'S17', 'A16', 'B16', 'C16', 'E16', 'F16', 'H16', 'I16', 'J16', 'K16', 'L16', 'M16', 'N16', 'O16', 'P16', 'Q16', 'R16', 'S16', 'A15', 'B15', 'C15', 'D15', 'E15', 'G15', 'H15', 'I15', 'J15', 'K15', 'L15', 'M15', 'N15', 'O15', 'P15', 'Q15', 'R15', 'S15', 'A14', 'B14', 'C14', 'D14', 'E14', 'F14', 'G14', 'H14', 'I14', 'J14', 'K14', 'L14', 'M14', 'N14', 'O14', 'P14', 'Q14', 'R14', 'S14', 'A13', 'B13', 'C13', 'D13', 'E13', 'F13', 'G13', 'H13', 'I13', 'J13', 'K13', 'L13', 'M13', 'N13', 'O13', 'P13', 'Q13', 'R13', 'S13', 'A12', 'B12', 'C12', 'D12', 'E12', 'F12', 'G12', 'H12', 'I12', 'J12', 'K12', 'M12', 'N12', 'O12', 'P12', 'Q12', 'R12', 'S12', 'A11', 'B11', 'C11', 'D11', 'E11', 'F11', 'G11', 'H11', 'I11', 'J11', 'K11', 'L11', 'M11', 'N11', 'O11', 'P11', 'Q11', 'R11', 'S11', 'A10', 'B10', 'C10', 'D10', 'E10', 'F10', 'G10', 'H10', 'I10', 'K10', 'L10', 'M10', 'N10', 'O10', 'P10', 'Q10', 'R10', 'S10', 'A9', 'B9', 'C9', 'D9', 'E9', 'F9', 'G9', 'H9', 'J9', 'K9', 'L9', 'M9', 'N9', 'O9', 'P9', 'Q9', 'R9', 'S9', 'A8', 'B8', 'C8', 'D8', 'E8', 'F8', 'G8', 'I8', 'J8', 'K8', 'L8', 'M8', 'N8', 'O8', 'P8', 'Q8', 'R8', 'S8', 'A7', 'B7', 'C7', 'D7', 'E7', 'F7', 'G7', 'H7', 'I7', 'J7', 'K7', 'L7', 'M7', 'N7', 'O7', 'P7', 'Q7', 'R7', 'S7', 'A6', 'B6', 'C6', 'D6', 'E6', 'F6', 'G6', 'H6', 'I6', 'J6', 'K6', 'L6', 'M6', 'N6', 'O6', 'Q6', 'R6', 'S6', 'A5', 'B5', 'C5', 'D5', 'E5', 'F5', 'G5', 'H5', 'I5', 'J5', 'K5', 'L5', 'M5', 'N5', 'O5', 'Q5', 'R5', 'S5', 'A4', 'B4', 'C4', 'D4', 'H4', 'I4', 'J4', 'K4', 'L4', 'M4', 'N4', 'O4', 'P4', 'Q4', 'R4', 'S4', 'A3', 'B3', 'C3', 'E3', 'F3', 'G3', 'H3', 'I3', 'J3', 'K3', 'L3', 'M3', 'N3', 'O3', 'P3', 'Q3', 'R3', 'S3', 'A2', 'B2', 'C2', 'E2', 'F2', 'G2', 'H2', 'I2', 'J2', 'K2', 'L2', 'M2', 'N2', 'O2', 'P2', 'Q2', 'R2', 'S2', 'A1', 'B1', 'C1', 'E1', 'F1', 'G1', 'H1', 'I1', 'J1', 'K1', 'L1', 'M1', 'N1', 'O1', 'P1', 'Q1', 'R1', 'S1']).

test(get_no_stones_on_board) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_no_stones_on_board(Board, NoStones),
    assertion(NoStones == 19),
    read_game_state('serials/1.pl', GameState1),
    get_board(GameState1, Board1),
    get_no_stones_on_board(Board1, NoStones1),
    assertion(NoStones1 == 15).

test(get_all_board_columns) :-
    TestBoard1 = [[a,b,c], [d,e,f], [g,h,i]],
    get_all_board_columns(TestBoard1, AllBoardColumns1),
    assertion(AllBoardColumns1 == [[a,d,g], [b,e,h], [c,f,i]]),
    TestBoard2 = [[a,b,c], [d,e,f], [g,h,i], [j,k,l]],
    get_all_board_columns(TestBoard2, AllBoardColumns2),
    assertion(AllBoardColumns2 == [[a,d,g,j], [b,e,h,k], [c,f,i,l]]).

test(get_neighboring_stones) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_neighboring_stones(Board, 'A1', NeighboringStonesA1),
    assertion(NeighboringStonesA1 == []),
    get_neighboring_stones(Board, 'D4', NeighboringStonesD2),
    assertion(NeighboringStonesD2 == [w,w]),
    get_neighboring_stones(Board, 'F3', NeighboringStonesF3),
    assertion(NeighboringStonesF3 == [w,b,w]),
    get_neighboring_stones(Board, 'd18', NeighboringStonesD18),
    assertion(NeighboringStonesD18 == [b,b]).

test(get_up_right_diagonal) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_up_right_diagonal(Board, 'A19', UpRightDiagonalA1),
    assertion(UpRightDiagonalA1 == []),
    get_up_right_diagonal(Board, 'C16', UpRightDiagonalC16),
    assertion(UpRightDiagonalC16 == [b,b,o]),
    get_up_right_diagonal(Board, 'g7', UpRightDiagonalG7),
    assertion(UpRightDiagonalG7 == [w,w,w,o,w,o,o,o,o,o,o,o]).

test(get_up_left_diagonal) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_up_left_diagonal(Board, 'A19', UpLeftDiagonalA1),
    assertion(UpLeftDiagonalA1 == []),
    get_up_left_diagonal(Board, 'H15', UpLeftDiagonalH15),
    assertion(UpLeftDiagonalH15 == [w,b,b,o]).

test(get_down_right_diagonal) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_down_right_diagonal(Board, 'C3', DownRightDiagonalC3),
    assertion(DownRightDiagonalC3 == [w,o]).

test(get_down_left_diagonal) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_down_left_diagonal(Board, 'F19', DownLeftDiagonalF19),
    assertion(DownLeftDiagonalF19 == [b,b, o,o,o]).

test(get_positive_diagonal) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_positive_diagonal(Board, 'J10', PositiveDiagonalJ10),
    assertion(PositiveDiagonalJ10 == [o,o,o,o,o,o,o,w,w,w,o,w,o,o,o,o,o,o,o]),
    get_positive_diagonal(Board, 'A19', PositiveDiagonalA19),
    assertion(PositiveDiagonalA19 == [o]).

test(get_negative_diagonal) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_negative_diagonal(Board, 'J10', NegativeDiagonalJ10),
    assertion(NegativeDiagonalJ10 == [o,o,o,w,o,o,o,o,o,w,o,o,o,o,o,o,o,o,o]),
    get_negative_diagonal(Board, 'A19', NegativeDiagonalA19),
    assertion(NegativeDiagonalA19 == NegativeDiagonalJ10),
    get_negative_diagonal(Board, 'A1', NegativeDiagonalA1),
    assertion(NegativeDiagonalA1 == [o]).

test(get_first_row_positions) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_first_row_positions(Board, FirstRowPositions),
    ExpectedFirstRowPositions = ['A1', 'B1', 'C1', 'D1', 'E1', 'F1', 'G1', 'H1', 'I1', 'J1', 'K1', 'L1', 'M1', 'N1', 'O1', 'P1', 'Q1', 'R1', 'S1'],
    sort(FirstRowPositions, SortedFirstRowPositions),
    sort(ExpectedFirstRowPositions, SortedExpectedFirstRowPositions),
    assertion(SortedFirstRowPositions == SortedExpectedFirstRowPositions).

test(get_first_column_positions) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_first_column_positions(Board, FirstColumnPositions),
    ExpectedFirstColumnPositions = ['A1', 'A2', 'A3', 'A4', 'A5', 'A6', 'A7', 'A8'
    , 'A9', 'A10', 'A11', 'A12', 'A13', 'A14', 'A15', 'A16', 'A17', 'A18'
    , 'A19'],
    sort(FirstColumnPositions, SortedFirstColumnPositions),
    sort(ExpectedFirstColumnPositions, SortedExpectedFirstColumnPositions),
    assertion(SortedFirstColumnPositions == SortedExpectedFirstColumnPositions).


test(get_all_positive_diagonal_starts) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_all_positive_diagonal_starts(Board, AllPositiveDiagonalStarts1),
    ExpectedPositiveDiagonalStarts1 = ['S1', 'R1', 'Q1', 'P1', 'O1', 'N1', 'M1', 'L1', 'K1', 'J1', 'I1', 'H1', 'G1', 'F1', 'E1'
    , 'D1', 'C1', 'B1', 'A19', 'A18', 'A17', 'A16', 'A15', 'A14', 'A13', 'A12', 'A11', 'A10',
    'A9', 'A8', 'A7', 'A6', 'A5', 'A4', 'A3', 'A2', 'A1'],
    sort(AllPositiveDiagonalStarts1, SortedAllPositiveDiagonalStarts1),
    sort(ExpectedPositiveDiagonalStarts1, SortedExpectedPositiveDiagonalStarts1),
    assertion(SortedAllPositiveDiagonalStarts1 == SortedExpectedPositiveDiagonalStarts1).

test(get_all_negative_diagonal_starts) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_all_negative_diagonal_starts(Board, AllNegativeDiagonalStarts1),
    ExpectedNegativeDiagonalStarts1 = ['S19', 'R19', 'Q19', 'P19', 'O19', 'N19', 'M19', 'L19', 'K19', 'J19', 'I19', 'H19', 'G19', 'F19', 'E19', 'D19', 'C19', 'B19', 'A19', 'A18', 'A17', 'A16', 'A15', 'A14', 'A13', 'A12', 'A11', 'A10', 'A9', 'A8', 'A7', 'A6', 'A5', 'A4', 'A3', 'A2', 'A1'],
    sort(AllNegativeDiagonalStarts1, SortedAllNegativeDiagonalStarts1),
    sort(ExpectedNegativeDiagonalStarts1, SortedExpectedNegativeDiagonalStarts1),
    assertion(SortedAllNegativeDiagonalStarts1 == SortedExpectedNegativeDiagonalStarts1).

test(get_all_positive_diagonals) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_all_positive_diagonals(Board, AllPositiveDiagonals1),
    ExpectedPositiveDiagonals1 = [['o'], ['o', 'o'], ['o', 'o', 'o'], ['o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'b', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'b', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['b', 'o', 'o', 'b', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'w', 'o', 'w', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'w', 'w', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o'], ['o', 'o'], ['o', 'o', 'o'], ['o', 'o', 'o', 'o'], ['o', 'o', 'o', 'b', 'o'], ['o', 'o', 'o', 'b', 'b', 'o'], ['o', 'o', 'o', 'w', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'b', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'b', 'w', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o'], ['o', 'o', 'o', 'o', 'o', 'o', 'o', 'w', 'w', 'w', 'o', 'w', 'o', 'o', 'o', 'o', 'o', 'o', 'o']],
    sort(AllPositiveDiagonals1, SortedAllPositiveDiagonals1),
    sort(ExpectedPositiveDiagonals1, SortedExpectedPositiveDiagonals1),
    assertion(SortedAllPositiveDiagonals1 == SortedExpectedPositiveDiagonals1).

test(get_all_negative_diagonals) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_all_negative_diagonals(Board, AllNegativeDiagonals1),
    ExpectedNegativeDiagonals1 = [['o'], ['o','o'], ['o','o','o'], ['o','o','o','o'], ['o','o','o','o','o'], ['o','o','o','o','o','o'], ['o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','w','o','o','o','o','o','o','o'], ['o','b','b','w','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','b','o','o','o','o','o','o','o','o','o','o','o','b','o','o','o'], ['o','o','b','o','b','o','o','o','o','o','o','o','o','o','b','o','o','o'], ['o','o','o','w','o','o','o','o','o','w','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','w','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','w','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','b','o','o','o'], ['o','o','o','o','o','w','o','o','o'], ['o','o','o','o','w','o','o','o'], ['o','o','o','o','o','o','o'], ['o','o','o','w','o','o'], ['o','o','o','w','o'], ['o','o','o','b'], ['o','o','o'], ['o','o'], ['o']],
    sort(AllNegativeDiagonals1, SortedAllNegativeDiagonals1),
    sort(ExpectedNegativeDiagonals1, SortedExpectedNegativeDiagonals1),
    assertion(SortedAllNegativeDiagonals1 == SortedExpectedNegativeDiagonals1).

test(get_all_diagonals) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_all_diagonals(Board, AllDiagonals1),
    ExpectedDiagonals1 = [['o'], ['o','o'], ['o','o','o'], ['o','o','o','o'], ['o','o','o','o','o'], ['o','o','o','o','o','o'], ['o','o','o','o','o','o','o'], ['o','o','o','o','b','o','o','o'], ['o','o','o','o','o','b','o','o','o'], ['o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['b','o','o','b','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','w','o','w','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','w','w','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o'], ['o','o'], ['o','o','o'], ['o','o','o','o'], ['o','o','o','b','o'], ['o','o','o','b','b','o'], ['o','o','o','w','o','o','o'], ['o','o','o','o','o','b','o','o'], ['o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','b','w','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','w','w','w','o','w','o','o','o','o','o','o','o'], ['o'], ['o','o'], ['o','o','o'], ['o','o','o','o'], ['o','o','o','o','o'], ['o','o','o','o','o','o'], ['o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','w','o','o','o','o','o','o','o'], ['o','b','b','w','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','b','o','o','o','o','o','o','o','o','o','o','o','b','o','o','o'], ['o','o','b','o','b','o','o','o','o','o','o','o','o','o','b','o','o','o'], ['o','o','o','w','o','o','o','o','o','w','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','w','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','w','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','b','o','o','o'], ['o','o','o','o','o','w','o','o','o'], ['o','o','o','o','w','o','o','o'], ['o','o','o','o','o','o','o'], ['o','o','o','w','o','o'], ['o','o','o','w','o'], ['o','o','o','b'], ['o','o','o'], ['o','o'], ['o']],
    sort(AllDiagonals1, SortedAllDiagonals1),
    sort(ExpectedDiagonals1, SortedExpectedDiagonals1),
    assertion(SortedAllDiagonals1 == SortedExpectedDiagonals1).

test(get_all_board_sequences) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_all_board_sequences(Board, AllBoardSequences1),
    ExpectedSequences1= [['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','b','b','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','b','o','b','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','w','o','o','w','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','b','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','w','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','w','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','w','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','w','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','b','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','b','o','o','o'], ['o','o','o','o','w','w','b','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','w','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','w','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','b','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','b','b','w','o','o','o','o','o','o','o','o','o','o','o','o','w','w','b'], ['o','b','o','o','o','o','o','o','o','o','o','o','o','o','o','w','o','o','o'], ['o','o','b','o','b','o','o','o','o','o','o','o','o','o','o','w','o','o','o'], ['o','o','o','w','o','o','o','o','o','o','o','o','o','o','o','b','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','w','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','w','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','w','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','w','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','b','b','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o'], ['o','o'], ['o','o','o'], ['o','o','o','o'], ['o','o','o','o','o'], ['o','o','o','o','o','o'], ['o','o','o','o','o','o','o'], ['o','o','o','o','b','o','o','o'], ['o','o','o','o','o','b','o','o','o'], ['o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['b','o','o','b','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','w','o','w','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','w','w','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o'], ['o','o'], ['o','o','o'], ['o','o','o','o'], ['o','o','o','b','o'], ['o','o','o','b','b','o'], ['o','o','o','w','o','o','o'], ['o','o','o','o','o','b','o','o'], ['o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','b','w','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','w','w','w','o','w','o','o','o','o','o','o','o'], ['o'], ['o','o'], ['o','o','o'], ['o','o','o','o'], ['o','o','o','o','o'], ['o','o','o','o','o','o'], ['o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','w','o','o','o','o','o','o','o'], ['o','b','b','w','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','b','o','o','o','o','o','o','o','o','o','o','o','b','o','o','o'], ['o','o','b','o','b','o','o','o','o','o','o','o','o','o','b','o','o','o'], ['o','o','o','w','o','o','o','o','o','w','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','w','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','w','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','o','o','o','o','o'], ['o','o','o','o','o','o','b','o','o','o'], ['o','o','o','o','o','w','o','o','o'], ['o','o','o','o','w','o','o','o'], ['o','o','o','o','o','o','o'], ['o','o','o','w','o','o'], ['o','o','o','w','o'], ['o','o','o','b'], ['o','o','o'], ['o','o'], ['o']],
    sort(AllBoardSequences1, SortedAllBoardSequences1),
    sort(ExpectedSequences1, SortedExpectedSequences1),
    assertion(SortedAllBoardSequences1 == SortedExpectedSequences1).

test(get_all_stone_sequences) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    get_all_stone_sequences(Board, 'white', AllStoneSequences1),
    ExpectedSequences = [['w'], ['w'], ['w'], ['w'], ['w'], ['w'], ['w','w'], ['w'], ['w'], ['w'], ['w','w'], ['w'], ['w'], ['w'], ['w'], ['w'], ['w'], ['w'], ['w'], ['w'], ['w','w'], ['w'], ['w'], ['w','w','w'], ['w'], ['w'], ['w'], ['w'], ['w'], ['w'], ['w'], ['w'], ['w'], ['w'], ['w']] ,
    length(AllStoneSequences1, ActualLength),
    length(ExpectedSequences, ExpectedLength),
    assertion(ActualLength == ExpectedLength),
    sort(AllStoneSequences1, SortedAllStoneSequences1),
    sort(ExpectedSequences, SortedExpectedSequences),
    assertion(SortedAllStoneSequences1 == SortedExpectedSequences),
    get_all_stone_sequences(Board, 'black', AllStoneSequences2),
    ExpectedSequence2 = [['b','b'], ['b'], ['b'], ['b'], ['b'], ['b'], ['b'], ['b'], ['b','b'], ['b'], ['b'], ['b'], ['b'], ['b'], ['b','b'], ['b'], ['b'], ['b'], ['b'], ['b'], ['b','b'], ['b'], ['b'], ['b','b'], ['b'], ['b'], ['b'], ['b'], ['b'], ['b'], ['b']] ,
    length(AllStoneSequences2, ActualLength2),
    length(ExpectedSequence2, ExpectedLength2),
    assertion(ActualLength2 == ExpectedLength2),
    sort(AllStoneSequences2, SortedAllStoneSequences2),
    sort(ExpectedSequence2, SortedExpectedSequence2),
    assertion(SortedAllStoneSequences2 == SortedExpectedSequence2).

test(get_empty_board) :-
    get_empty_board(19, 19, EmptyBoard),
    get_board_size(EmptyBoard, NoRows, NoCols),
    assertion(NoRows == 19),
    assertion(NoCols == 19),
    flatten(EmptyBoard, FlatEmptyBoard),
    sort(FlatEmptyBoard, SortedFlatEmptyBoard),
    assertion(SortedFlatEmptyBoard == ['o']).

test(get_center) :-
    get_empty_board(19, 19, EmptyBoard),
    get_center(EmptyBoard, Center),
    assertion(Center == 'J10'),
    get_empty_board(3, 3, EmptyBoard2),
    get_center(EmptyBoard2, Center2),
    assertion(Center2 == 'B2').

test(get_positions_3_away_from_center) :-
    get_empty_board(19, 19, EmptyBoard),
    get_positions_3_away_from_center(EmptyBoard, Positions3AwayFromCenter),
    ExpectedPositions3AwayFromCenter = ['G10','G11','G12','G13','G7','G8','G9','H13','H7','I13','I7','J13','J7','K13','K7','L13','L7','M10','M11','M12','M13','M7','M8','M9'],
    sort(Positions3AwayFromCenter, SortedPositions3AwayFromCenter),
    sort(ExpectedPositions3AwayFromCenter, SortedExpectedPositions3AwayFromCenter),
    assertion(SortedPositions3AwayFromCenter == SortedExpectedPositions3AwayFromCenter).


test(is_first_move) :-
    get_empty_board(19, 19, EmptyBoard),
    assertion(is_first_move(EmptyBoard)),
    set_stone(EmptyBoard, 'J10', 'white', Board1),
    assertion(\+ is_first_move(Board1)).

test(is_third_move) :-
    get_empty_board(19, 19, EmptyBoard),
    assertion(\+ is_third_move(EmptyBoard)),
    set_stone(EmptyBoard, 'J10', 'white', Board1),
    assertion(\+ is_third_move(Board1)),
    set_stone(Board1, 'J11', 'black', Board2),
    assertion(is_third_move(Board2)).

test(get_available_moves_first) :-
    get_empty_board(19, 19, EmptyBoard),
    get_available_moves(EmptyBoard, EmptyAvailableMoves),
    ExpectedEmptyAvailableMoves = ['J10'],
    sort(EmptyAvailableMoves, SortedEmptyAvailableMoves),
    sort(ExpectedEmptyAvailableMoves, SortedExpectedEmptyAvailableMoves),
    assertion(SortedEmptyAvailableMoves == SortedExpectedEmptyAvailableMoves).

test(get_available_moves_second) :-
    get_empty_board(19, 19, EmptyBoard),
    set_stone(EmptyBoard, 'J10', 'white', Board1),
    get_available_moves(Board1, AvailableMoves),
    get_all_positions(EmptyBoard, AllPositions),
    % Remove J10 from all positions
    delete(AllPositions, 'J10', ExpectedAvailableMoves),
    sort(AvailableMoves, SortedAvailableMoves),
    sort(ExpectedAvailableMoves, SortedExpectedAvailableMoves),
    assertion(SortedAvailableMoves == SortedExpectedAvailableMoves),
    LispCopiedAvailableMoves = ['A1','A10','A11','A12','A13','A14','A15','A16','A17','A18','A19','A2','A3','A4','A5','A6','A7','A8','A9','B1','B10','B11','B12','B13','B14','B15','B16','B17','B18','B19','B2','B3','B4','B5','B6','B7','B8','B9','C1','C10','C11','C12','C13','C14','C15','C16','C17','C18','C19','C2','C3','C4','C5','C6','C7','C8','C9','D1','D10','D11','D12','D13','D14','D15','D16','D17','D18','D19','D2','D3','D4','D5','D6','D7','D8','D9','E1','E10','E11','E12','E13','E14','E15','E16','E17','E18','E19','E2','E3','E4','E5','E6','E7','E8','E9','F1','F10','F11','F12','F13','F14','F15','F16','F17','F18','F19','F2','F3','F4','F5','F6','F7','F8','F9','G1','G10','G11','G12','G13','G14','G15','G16','G17','G18','G19','G2','G3','G4','G5','G6','G7','G8','G9','H1','H10','H11','H12','H13','H14','H15','H16','H17','H18','H19','H2','H3','H4','H5','H6','H7','H8','H9','I1','I10','I11','I12','I13','I14','I15','I16','I17','I18','I19','I2','I3','I4','I5','I6','I7','I8','I9','J1','J11','J12','J13','J14','J15','J16','J17','J18','J19','J2','J3','J4','J5','J6','J7','J8','J9','K1','K10','K11','K12','K13','K14','K15','K16','K17','K18','K19','K2','K3','K4','K5','K6','K7','K8','K9','L1','L10','L11','L12','L13','L14','L15','L16','L17','L18','L19','L2','L3','L4','L5','L6','L7','L8','L9','M1','M10','M11','M12','M13','M14','M15','M16','M17','M18','M19','M2','M3','M4','M5','M6','M7','M8','M9','N1','N10','N11','N12','N13','N14','N15','N16','N17','N18','N19','N2','N3','N4','N5','N6','N7','N8','N9','O1','O10','O11','O12','O13','O14','O15','O16','O17','O18','O19','O2','O3','O4','O5','O6','O7','O8','O9','P1','P10','P11','P12','P13','P14','P15','P16','P17','P18','P19','P2','P3','P4','P5','P6','P7','P8','P9','Q1','Q10','Q11','Q12','Q13','Q14','Q15','Q16','Q17','Q18','Q19','Q2','Q3','Q4','Q5','Q6','Q7','Q8','Q9','R1','R10','R11','R12','R13','R14','R15','R16','R17','R18','R19','R2','R3','R4','R5','R6','R7','R8','R9','S1','S10','S11','S12','S13','S14','S15','S16','S17','S18','S19','S2','S3','S4','S5','S6','S7','S8','S9'],
    sort(LispCopiedAvailableMoves, SortedLispCopiedAvailableMoves),
    length(SortedLispCopiedAvailableMoves, LispCopiedAvailableMovesLength),
    length(ExpectedAvailableMoves, ExpectedAvailableMovesLength),
    assertion(LispCopiedAvailableMovesLength == ExpectedAvailableMovesLength),
    assertion(SortedLispCopiedAvailableMoves == SortedExpectedAvailableMoves).


test(get_available_moves_third) :-
    get_empty_board(19, 19, EmptyBoard),
    set_stone(EmptyBoard, 'J10', 'white', Board1),
    set_stone(Board1, 'J11', 'black', Board2),
    
    ExpectedAvailableMoves = ['S19', 'R19', 'Q19', 'P19', 'O19', 'N19', 'M19', 'L19', 'K19', 'J19', 'I19', 'H19', 'G19', 'F19', 'E19', 'D19', 'C19', 'B19', 'A19', 'S18', 'R18', 'Q18', 'P18', 'O18', 'N18', 'M18', 'L18', 'K18', 'J18', 'I18', 'H18', 'G18', 'F18', 'E18', 'D18', 'C18', 'B18', 'A18', 'S17', 'R17', 'Q17', 'P17', 'O17', 'N17', 'M17', 'L17', 'K17', 'J17', 'I17', 'H17', 'G17', 'F17', 'E17', 'D17', 'C17', 'B17', 'A17', 'S16', 'R16', 'Q16', 'P16', 'O16', 'N16', 'M16', 'L16', 'K16', 'J16', 'I16', 'H16', 'G16', 'F16', 'E16', 'D16', 'C16', 'B16', 'A16', 'S15', 'R15', 'Q15', 'P15', 'O15', 'N15', 'M15', 'L15', 'K15', 'J15', 'I15', 'H15', 'G15', 'F15', 'E15', 'D15', 'C15', 'B15', 'A15', 'S14', 'R14', 'Q14', 'P14', 'O14', 'N14', 'M14', 'L14', 'K14', 'J14', 'I14', 'H14', 'G14', 'F14', 'E14', 'D14', 'C14', 'B14', 'A14', 'S13', 'R13', 'Q13', 'P13', 'O13', 'N13', 'M13', 'L13', 'K13', 'J13', 'I13', 'H13', 'G13', 'F13', 'E13', 'D13', 'C13', 'B13', 'A13', 'S12', 'R12', 'Q12', 'P12', 'O12', 'N12', 'M12', 'G12', 'F12', 'E12', 'D12', 'C12', 'B12', 'A12', 'S11', 'R11', 'Q11', 'P11', 'O11', 'N11', 'M11', 'G11', 'F11', 'E11', 'D11', 'C11', 'B11', 'A11', 'S10', 'R10', 'Q10', 'P10', 'O10', 'N10', 'M10', 'G10', 'F10', 'E10', 'D10', 'C10', 'B10', 'A10', 'S9', 'R9', 'Q9', 'P9', 'O9', 'N9', 'M9', 'G9', 'F9', 'E9', 'D9', 'C9', 'B9', 'A9', 'S8', 'R8', 'Q8', 'P8', 'O8', 'N8', 'M8', 'G8', 'F8', 'E8', 'D8', 'C8', 'B8', 'A8', 'S7', 'R7', 'Q7', 'P7', 'O7', 'N7', 'M7', 'L7', 'K7', 'J7', 'I7', 'H7', 'G7', 'F7', 'E7', 'D7', 'C7', 'B7', 'A7', 'S6', 'R6', 'Q6', 'P6', 'O6', 'N6', 'M6', 'L6', 'K6', 'J6', 'I6', 'H6', 'G6', 'F6', 'E6', 'D6', 'C6', 'B6', 'A6', 'S5', 'R5', 'Q5', 'P5', 'O5', 'N5', 'M5', 'L5', 'K5', 'J5', 'I5', 'H5', 'G5', 'F5', 'E5', 'D5', 'C5', 'B5', 'A5', 'S4', 'R4', 'Q4', 'P4', 'O4', 'N4', 'M4', 'L4', 'K4', 'J4', 'I4', 'H4', 'G4', 'F4', 'E4', 'D4', 'C4', 'B4', 'A4', 'S3', 'R3', 'Q3', 'P3', 'O3', 'N3', 'M3', 'L3', 'K3', 'J3', 'I3', 'H3', 'G3', 'F3', 'E3', 'D3', 'C3', 'B3', 'A3', 'S2', 'R2', 'Q2', 'P2', 'O2', 'N2', 'M2', 'L2', 'K2', 'J2', 'I2', 'H2', 'G2', 'F2', 'E2', 'D2', 'C2', 'B2', 'A2', 'S1', 'R1', 'Q1', 'P1', 'O1', 'N1', 'M1', 'L1', 'K1', 'J1', 'I1', 'H1', 'G1', 'F1', 'E1', 'D1', 'C1', 'B1', 'A1'],
    get_available_moves(Board2, AvailableMoves),
    sort(AvailableMoves, SortedAvailableMoves),
    sort(ExpectedAvailableMoves, SortedExpectedAvailableMoves),
    assertion(SortedAvailableMoves == SortedExpectedAvailableMoves).

test(get_sequence_score) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board4),
    get_sequence_score(Board4, 'white', SeqScore4White1),
    assertion(SeqScore4White1 == 0),
    get_sequence_score(Board4, 'black', SeqScore4Black1),
    assertion(SeqScore4Black1 == 0),
    set_stone(Board4, 'K11', 'white', Board4White2),
    get_sequence_score(Board4White2, 'white', SeqScore4White2),
    assertion(SeqScore4White2 == 5).

test(get_sequence_score_serial_3) :-
    read_game_state('serials/3.pl', GameState),
    get_board(GameState, Board3),
    get_sequence_score(Board3, 'white', SeqScore3White1),
    assertion(SeqScore3White1 == 0),
    get_sequence_score(Board3, 'black', SeqScore3Black1),
    assertion(SeqScore3Black1 == 0),
    set_stone(Board3, 'H5', 'b', Board3Black2),
    set_stone(Board3Black2, 'I5', 'b', Board3Black3),
    get_sequence_score(Board3Black3, 'black', SeqScore3Black2),    
    assertion(SeqScore3Black2 == 5),
    set_stone(Board3Black3, 'F2', 'w', Board3White2),
    set_stone(Board3White2, 'G2', 'w', Board3White3),
    get_sequence_score(Board3White3, 'white', SeqScore3White2),
    assertion(SeqScore3White2 == 1),
    set_stone(Board3White3, 'F1', 'w', Board3White4),
    get_sequence_score(Board3White4, 'white', SeqScore3White3),
    assertion(SeqScore3White3 == 2),
    set_stone(Board3White4, 'H2', 'w', Board3White5),
    get_sequence_score(Board3White5, 'white', SeqScore3White4),
    assertion(SeqScore3White4 == 6).



:-end_tests(board).

:- run_tests(board).