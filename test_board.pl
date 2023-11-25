:- begin_tests(board).

% Consult board.pl
:- consult(board).
:- consult(game_state).

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

:-end_tests(board).

:- run_tests(board).