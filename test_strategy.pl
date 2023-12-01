:- begin_tests(strategy).

:-use_module(board).
:-use_module(game_state).
:-use_module(strategy).

test(first_move_is_the_only_move) :-
    get_initial_state(InitialState),
    set_current_player(InitialState, 'human', State),
    only_move(State, 'J10'),
    \+ only_move(State, 'J9').

test(capturing_move_s1) :-
    read_game_state('serials/1.pl', Serial1),
    capturing_move(Serial1, 'J12'),
    \+ capturing_move(Serial1, 'A1').

test(capturing_move_s2) :-
    read_game_state('serials/2.pl', Serial2),
    capturing_move(Serial2, 'I9'),
    \+ capturing_move(Serial2, 'A1').

test(capture_blocking_move) :-
    read_game_state('serials/1.pl', Serial1),
    switch_turn(Serial1, Serial2),
    capture_blocking_move(Serial2, 'J12').

test(winning_move) :-
    read_game_state('serials/3.pl', Serial1),
    \+ winning_move(Serial1, 'A1'),
    \+ winning_move(Serial1, 'F12'),
    switch_turn(Serial1, Serial2),
    winning_move(Serial2, 'F12').

test(sequence_making_move) :-
    read_game_state('serials/4.pl', Serial1),
    sequence_making_move(Serial1, 'D4'),
    \+ sequence_making_move(Serial1, 'F12').

test(sequence_blocking_move) :-
    read_game_state('serials/4.pl', Serial1),
    switch_turn(Serial1, Serial2),
    sequence_blocking_move(Serial2, 'K11'),
    \+ sequence_blocking_move(Serial2, 'F12').

test(win_blocking_move)


:- end_tests(strategy).

:- run_tests(strategy).
