:- module(pente, [play_pente/0]).

:- use_module(tournament).
:- use_module(human).
:- use_module(game_state).
:- use_module(round).

% play_pente/0
% Starts the game of Pente.
play_pente :-
    human_wants_to_load_game,
    load_game_state_from_human_input(GameState),
    play_tournament(GameState);
    true.

play_pente :-
    get_initial_state(GameState),
    set_starting_player(GameState, NewGameState),
    play_tournament(NewGameState);
    true.

:- play_pente.