:- module(computer,
    [get_computer_move/2]
    ).

:-use_module(game_state).
:-use_module(board).
:-use_module(strategy).

% get_computer_move(+GameState, -Move)
% Returns a random valid move for the computer
get_computer_move(GameState, Move):-
    get_best_move(GameState, Move),
    get_move_rationale(GameState, Move, Rationale),
    format('Computer\'s move: ~w~n', [Move]),
    format('Rationale: ~w~n', [Rationale]).
