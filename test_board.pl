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


:-end_tests(board).

:- run_tests(board).