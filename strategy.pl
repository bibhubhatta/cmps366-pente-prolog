:- module(strategy,
    [
        get_best_move/2,
        get_pseudo_score/3,
        get_pseudo_sequence_score/3
    ]
    ).

:- use_module(game_state).
:- use_module(board).


% get_pseudo_sequence_score(+GameState, +Player, -Score)
% Calculates the pseudo score for the given player's sequences
% The pseudo score is calculated by summing the square of the length of sequences that are longer than 1
% https://www.google.com/search?q=swipl%20sum_list
get_pseudo_sequence_score(GameState, Player, Score) :-
    get_stone_from_player(GameState, Player, Stone),
    get_board(GameState, Board),
    get_all_stone_sequences(Board, Stone, Sequences),
    include(length_greater_than_or_equal_to(2), Sequences, LongSequences),
    maplist(length, LongSequences, Lengths),
    maplist(square, Lengths, Squares),
    append(Squares, [0], SquaresWithZero),
    sum_list(SquaresWithZero, Score).

% square(Number, Square)
% Helper predicate to square a number
square(Number, Square) :-
    Square is Number * Number.

% get_pseudo_score(+GameState, +Move, -Score)
% Calculates the pseudo score for the given move
% The pseudo score is calculated by adding the score for the player and the opponent if the move is played by both
get_pseudo_score(GameState, Move, Score) :-
    get_current_player(GameState, CurrentPlayer),
    other_player(CurrentPlayer, Opponent),
    make_move(GameState, Move, GameStateAfterMove),
    switch_turn(GameState, GameStateIfOpponentMove),
    make_move(GameStateIfOpponentMove, Move, GameStateIfOpponentMoveAfterMove),
    get_round_score(GameStateAfterMove, CurrentPlayer, CurrentPlayerScore),
    get_round_score(GameStateIfOpponentMoveAfterMove, Opponent, OpponentScore),
    get_pseudo_sequence_score(GameStateAfterMove, CurrentPlayer, CurrentPlayerPseudoScore),
    get_pseudo_sequence_score(GameStateIfOpponentMoveAfterMove, Opponent, OpponentPseudoScore),
    Score is (CurrentPlayerScore * 1000) + (OpponentScore * 1000) + (CurrentPlayerPseudoScore * 10) + (OpponentPseudoScore * 10).

% get_pseudo_scores(+GameState, +Moves, -Scores)
% Calculates the pseudo scores for the given moves
% Returns a list of the list of moves and their pseudo scores
get_pseudo_scores(GameState, Moves, Scores) :-
    findall([Move, Score], (member(Move, Moves), get_pseudo_score(GameState, Move, Score)), Scores).

% get_random_move(+GameState, -Move)
% Returns a random valid move
get_random_move(GameState, Move):-
    get_board(GameState, Board),
    get_available_moves(Board, ValidMoves),
    length(ValidMoves, Length),
    random(0, Length, Index),
    nth0(Index, ValidMoves, Move).

% get_best_move(+GameState, -BestMove)
% To get the best move for the game state
% The best move is the one with the highest pseudo score. The pseudo score is calculated for available moves, and sorted to find the one with the highest score.
% https://stackoverflow.com/questions/25124122/swi-prolog-how-to-sort-list-of-lists-by-nth-element-of-sublist-allowing-duplic
get_best_move(GameState, BestMove) :-
    get_board(GameState, Board),
    get_available_moves(Board, AvailableMoves),
    get_pseudo_scores(GameState, AvailableMoves, Scores),
    sort(2, @>=, Scores, SortedScores),
    SortedScores = [[BestMove, _]|_].
    % get_random_move(GameState, BestMove).