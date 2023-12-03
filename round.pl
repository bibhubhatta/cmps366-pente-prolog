:- module(round,
    [
        conduct_round/2,
        announce_round_result/1,
        play_round/2,
        get_move/2,
        set_starting_player/2
    ]
    ).

:- use_module(board).    
:- use_module(game_state).
:- use_module(human).
:- use_module(computer).


% conduct_round(+GameState, -FinalGameState)
% Predicate to conduct a round of the game.
% GameState is the initial state of the game.
% FinalGameState is the final state of the game.
conduct_round(GameState, FinalGameState) :-
    print_round_state(GameState),
    play_round(GameState, FinalGameState),
    (
        is_game_over(FinalGameState) -> announce_round_result(FinalGameState); true
    ).

% announce_round_result(+GameState)
% Predicate to announce the result of a round.
announce_round_result(GameState) :-
    % Print the final board state.
    get_board(GameState, Board),
    cartesian_board(Board, CartesianBoard),
    print_board(CartesianBoard),
    nl,
    nl,
    format('---------- Round Results ----------~n'),
    (
    get_winner(GameState, Winner) -> format('~w wins the round!~n', [Winner]);
    is_game_drawn(GameState) -> format('The round is drawn!~n')
    ),
    get_round_score(GameState, human, HumanScore),
    get_round_score(GameState, computer, ComputerScore),
    format('Human\'s round score: ~w~n', [HumanScore]),
    format('Computer\'s round score: ~w~n', [ComputerScore]),
    format('-----------------------------------~n'),
    nl,
    nl.

% play_round(+GameState, -FinalGameState)
% Predicate to play a round of the game.
% GameState is the initial state of the game.
% FinalGameState is the final state of the game.
play_round(GameState, FinalGameState) :-
    is_game_over(GameState),
    FinalGameState = GameState.

play_round(GameState, FinalGameState) :-
    print_round_state(GameState),
    human_wants_to_save_and_quit,
    save_to_human_location(GameState),
    FinalGameState = GameState.

play_round(GameState, FinalGameState) :-
    print_round_state(GameState),
    get_move(GameState, Move),
    make_move(GameState, Move, NewGameState),
    play_round(NewGameState, FinalGameState).

% get_move(+GameState, -Move)
% Predicate to get a move from the current player.
% GameState is the current state of the game.
% Move is the move made by the current player.
get_move(GameState, Move) :-
    get_current_player(GameState, human),
    get_human_move(GameState, Move).

get_move(GameState, Move) :-
    get_current_player(GameState, computer),
    get_computer_move(GameState, Move).

% set_starting_player(+GameState, -NewGameState)
% Predicate to set the starting player for the round.
% GameState is the current state of the game.
% NewGameState is the new state of the game with the starting player set.
set_starting_player(GameState, NewGameState) :-
    % Do nothing if the starting player is already set.
    get_current_player(GameState, CurrentPlayer),
    not(CurrentPlayer = '_'),
    NewGameState = GameState.

% Otherwise, set the starting player.
set_starting_player(GameState, NewGameState) :-
    get_player_tournament_score(GameState, human, HumanScore),
    get_player_tournament_score(GameState, computer, ComputerScore),
    HumanScore > ComputerScore,
    set_current_player(GameState, human, NewGameState).

set_starting_player(GameState, NewGameState) :-
    get_player_tournament_score(GameState, human, HumanScore),
    get_player_tournament_score(GameState, computer, ComputerScore),
    ComputerScore > HumanScore,
    set_current_player(GameState, computer, NewGameState).

set_starting_player(GameState, NewGameState) :-
    % If the scores are equal, toss a coin.
    get_player_tournament_score(GameState, human, HumanScore),
    get_player_tournament_score(GameState, computer, ComputerScore),
    HumanScore = ComputerScore,
    human_wins_toss,
    set_current_player(GameState, human, NewGameState).

set_starting_player(GameState, NewGameState) :-
    set_current_player(GameState, computer, NewGameState).

% print_round_state(+GameState)
% Predicate to print the current state of the game.
print_round_state(GameState) :-
    nl,
    write('Board:'), nl,
    get_board(GameState, Board),
    cartesian_board(Board, CartesianBoard),
    print_board(CartesianBoard),
    nl,
    get_player_captures(GameState, human, HumanCaptures),
    format('Human captures: ~w~n', [HumanCaptures]),
    get_player_captures(GameState, computer, ComputerCaptures),
    format('Computer captures: ~w~n', [ComputerCaptures]),
    get_round_score(GameState, human, HumanScore),
    format('Human round score: ~w~n', [HumanScore]),
    get_round_score(GameState, computer, ComputerScore),
    format('Computer round score: ~w~n', [ComputerScore]),
    get_current_player(GameState, CurrentPlayer),
    format('Current player: ~w~n', [CurrentPlayer]),
    get_current_player_stone(GameState, CurrentStone),
    format('Current stone: ~w~n', [CurrentStone]),
    nl.