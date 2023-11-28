:- ensure_loaded('test_game_state.pl').
:- ensure_loaded('test_position.pl').
:- ensure_loaded('test_board.pl').

% Manual tests for I/O 
:- nl.
:- nl.

:- use_module(tournament).
:- use_module(game_state).

:- read_game_state('serials/1.pl', GameState),
    play_tournament(GameState).

:- halt.