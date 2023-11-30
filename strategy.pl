:- module(strategy,
    [
        get_best_move/2,
        get_pseudo_score/3,
        get_pseudo_sequence_score/3,
        winning_move/2,
        win_blocking_move/2,
        capturing_move/2,
        capture_blocking_move/2,
        only_move/2,
        sequence_making_move/2,
        sequence_blocking_move/2,
        get_rationale_explanation/2,
        get_move_rationale/3,
        get_explanation_from_rationales/2
    ]
    ).

:- use_module(game_state).
:- use_module(board).
:- use_module(position).


% win_move(+GameState, +Move)
% Checks if the move is a winning move
winning_move(GameState, Move) :-
    make_move(GameState, Move, NewGameState),
    get_winner(NewGameState, _).

% win_blocking_move(+GameState, +Move)
% Checks if the move prevents the opponent from winning
win_blocking_move(GameState, Move) :-
    switch_turn(GameState, SwitchedGameState),
    make_move(SwitchedGameState, Move, NewGameState),
    get_winner(NewGameState, _).

% capturing_move(+GameState, +Move)
% Checks if the move captures a stone
capturing_move(GameState, Move) :-
    get_current_player(GameState, CurrentPlayer),
    % other_player(CurrentPlayer, Opponent),
    make_move(GameState, Move, GameStateAfterMove),
    get_player_captures(GameState, CurrentPlayer, CapturesBefore),
    get_player_captures(GameStateAfterMove, CurrentPlayer, CapturesAfter),
    CapturesBefore \= CapturesAfter.

% capture_blocking_move(+GameState, +Move)
% Checks if the move prevents the opponent from capturing a stone
capture_blocking_move(GameState, Move) :-
    switch_turn(GameState, SwitchedGameState),
    capturing_move(SwitchedGameState, Move).

% only_move(+GameState, +Move)
% Checks if the move is the only move available
only_move(GameState, Move) :-
    get_board(GameState, Board),
    get_available_moves(Board, AvailableMoves),
    length(AvailableMoves, MovesCount),
    MovesCount =:= 1,
    nth0(0, AvailableMoves, Move).

% stone_in_list(+Stone, +List)
% Checks if the stone is in the list
stone_in_list(Stone, List) :-
    (   Stone = 'white' -> stone_in_list('w', List)
    ;   Stone = 'black' -> stone_in_list('b', List)
    ;   Stone = 'empty' -> stone_in_list('o', List)
    ;   member(Stone, List)
    ).

% sequence_making_move(+GameState, +Move)
% Checks if the move creates a sequence
sequence_making_move(GameState, Move) :-
    get_current_player_stone(GameState, CurrentStone),
    get_board(GameState, Board),
    get_neighboring_stones(Board, Move, Neighbors),
    stone_in_list(CurrentStone, Neighbors).

% sequence_blocking_move(+GameState, +Move)
% Checks if the move prevents the opponent from creating a sequence
sequence_blocking_move(GameState, Move) :-
    make_move(GameState, Move, GameStateAfterMove),
    get_current_player_stone(GameState, CurrentStone),
    other_stone(CurrentStone, OpponentStone),
    get_board(GameStateAfterMove, Board),
    get_neighboring_stones(Board, Move, Neighbors),
    stone_in_list(OpponentStone, Neighbors).

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
    maplist(cube, Lengths, LengthsExponiated),
    append(LengthsExponiated, [0], SquaresWithZero),
    sum_list(SquaresWithZero, Score).

% get_pseudo_sequence_score_optimized(+GameState, +Position, -Score)
% Calculates the pseudo score for the given player's sequences
% The pseudo score is calculated by summing the square of the length of sequences that are longer than 1
get_pseudo_sequence_score_optimized(GameState, Position, Score) :-
    get_board(GameState, Board),
    get_all_stone_sequences_localized(Board, Position, Sequences),
    % format('Position: ~w~n', [Position]),
    % format('Sequences: ~w~n', [Sequences]),
    include(length_greater_than_or_equal_to(2), Sequences, LongSequences),
    maplist(length, LongSequences, Lengths),
    maplist(cube, Lengths, LengthsExponiated),
    append(LengthsExponiated, [0], SquaresWithZero),
    sum_list(SquaresWithZero, Score).

% square(Number, Square)
% Helper predicate to square a number
square(Number, Square) :-
    Square is Number * Number.

% cube(Number, Cube)
% Helper predicate to cube a number
cube(Number, Cube) :-
    Cube is Number * Number * Number.


get_round_score_optimized(GameState, Position, Score) :-
    get_board(GameState, Board),
    get_all_stone_sequences_localized(Board, Position, Sequences),
    include(length_greater_than_or_equal_to(5), Sequences, FiveOrMoreSequencesSequences),
    length(FiveOrMoreSequencesSequences, FiveOrMoreSequencesCount),
    include(length_equal_to(4), Sequences, FourSequences),
    length(FourSequences, FourSequencesCount),
    SequenceScore is (FiveOrMoreSequencesCount * 5) + (FourSequencesCount * 1),
    get_current_player(GameState, CurrentPlayer),
    get_player_captures(GameState, CurrentPlayer, Captures),
    Score is SequenceScore + Captures.

% get_pseudo_score(+GameState, +Move, -Score)
% Calculates the pseudo score for the given move
% The pseudo score is calculated by adding the score for the player and the opponent if the move is played by both
get_pseudo_score(GameState, Move, Score) :-
    get_current_player(GameState, CurrentPlayer),
    other_player(CurrentPlayer, Opponent),
    make_move(GameState, Move, GameStateAfterMove),
    switch_turn(GameState, GameStateIfOpponentMove),
    make_move(GameStateIfOpponentMove, Move, GameStateIfOpponentMoveAfterMove),
    get_round_score_optimized(GameStateAfterMove, Move, CurrentPlayerScore),
    get_round_score_optimized(GameStateIfOpponentMoveAfterMove, Move, OpponentScore),
    get_pseudo_sequence_score_optimized(GameStateAfterMove, Move, CurrentPlayerPseudoScore),
    get_pseudo_sequence_score_optimized(GameStateIfOpponentMoveAfterMove, Move, OpponentPseudoScore),
    get_player_captures(GameStateAfterMove, CurrentPlayer, CurrentPlayerCaptures),
    get_player_captures(GameStateIfOpponentMoveAfterMove, Opponent, OpponentCaptures),
    get_distance_from_center(GameState, Move, DistanceFromCenter),
    Score is (CurrentPlayerScore * 10000) + 
             (OpponentScore * 10000) + 
             (CurrentPlayerCaptures * 1500) +
             (OpponentCaptures * 1000) +
             (CurrentPlayerPseudoScore * 15) + 
             (OpponentPseudoScore * 10) -
             (DistanceFromCenter).

% distance_from_center(+GameState, +Move, -Distance)
% Calculates the distance of the move from the center
get_distance_from_center(GameState, Move, Distance) :-
    get_board(GameState, Board),
    get_center(Board, CenterPosition),
    get_distance(CenterPosition, Move, Distance).

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
    % Filter only the moves with the highest score
    % Find the highest score
    maplist(nth0(1), Scores, ScoresList),
    max_list(ScoresList, MaxScore),
    % Filter the moves with the highest score
    findall(Move, (member([Move, Score], Scores), Score =:= MaxScore), BestMoves),
    % Randomly select one of the best moves
    length(BestMoves, Length),
    % writeln(Length),
    random(0, Length, Index),
    nth0(Index, BestMoves, BestMove).
    % get_random_move(GameState, BestMove).

% get_move_rationale(+GameState, +Move, -Explanation)
% Gets the explanation for the given move 
get_move_rationale(GameState, Move, Explanation) :-
    MoveAnalysisFunctions = ['only_move', 'winning_move', 'win_blocking_move', 'capturing_move', 'capture_blocking_move', 'sequence_making_move', 'sequence_blocking_move'],
    include(function_returns_true(GameState, Move), MoveAnalysisFunctions, RationaleList),
    get_explanation_from_rationales(RationaleList, Explanation).

% function_returns_true(+GameState, +Move, +Function)
% Helper predicate to check if the given function returns true for the given game state and move
function_returns_true(GameState, Move, Function) :-
    call(Function, GameState, Move).

% get_rationale_explanation(+Rationale, -Explanation)
% Defines the explanation for the given rationale
get_rationale_explanation(only_move, Explanation) :-
    Explanation = "The move is the only available move. ".
get_rationale_explanation(winning_move, Explanation) :-
    Explanation = "The move is a winning move. ".
get_rationale_explanation(win_blocking_move, Explanation) :-
    Explanation = "The move prevents the opponent from winning. ".
get_rationale_explanation(capturing_move, Explanation) :-
    Explanation = "The move is a capturing move. ".
get_rationale_explanation(capture_blocking_move, Explanation) :-
    Explanation = "The move prevents the opponent from capturing. ".
get_rationale_explanation(sequence_making_move, Explanation) :-
    Explanation = "The move is a sequence making move. ".
get_rationale_explanation(sequence_blocking_move, Explanation) :-
    Explanation = "The move prevents the opponent from making a sequence. ".
get_rationale_explanation(_, Explanation) :-
    Explanation = "The move is a random move. ".

% get_explanation_from_rationales(+Rationales, -Explanation)
% Constructs a human readable explanation from the list of rationales
get_explanation_from_rationales(Rationales, Explanation) :-
    % If rationale is empty, then it is a random move, otherwise, it is a combination of rationales
    Rationales = [] -> Explanation = "The move is a random move. ";
    maplist(get_rationale_explanation, Rationales, ExplanationList),
    atomic_list_concat(ExplanationList, Explanation).