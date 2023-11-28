:- ensure_loaded('test_game_state.pl').
:- ensure_loaded('test_position.pl').
:- ensure_loaded('test_board.pl').

% Manual tests for I/O 
:- nl.
:- nl.

:- ensure_loaded('round.pl').

:- read_game_state('serials/1.pl', GameState),
    conduct_round(GameState, NewGameState).

:- halt.