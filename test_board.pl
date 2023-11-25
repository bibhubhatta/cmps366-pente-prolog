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

:-end_tests(board).

:- run_tests(board).