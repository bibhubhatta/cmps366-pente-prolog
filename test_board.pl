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

test(print_board) :-
    read_game_state('serials/4.pl', GameState),
    get_board(GameState, Board),
    cartesian_board(Board, NewBoard),
    print_board(NewBoard).

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
    assertion(Stone_d2 == o).

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


:-end_tests(board).

:- run_tests(board).