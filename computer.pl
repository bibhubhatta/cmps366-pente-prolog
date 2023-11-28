:- module(computer,
    [get_computer_move/2]
    ).

:-use_module(game_state).
:-use_module(board).

% get_computer_move(+GameState, -Move)
% Returns a random valid move for the computer
get_computer_move(GameState, Move):-
    get_random_move(GameState, Move).

% get_random_move(+GameState, -Move)
% Returns a random valid move
get_random_move(GameState, Move):-
    get_board(GameState, Board),
    get_available_moves(Board, ValidMoves),
    length(ValidMoves, Length),
    random(0, Length, Index),
    nth0(Index, ValidMoves, Move).