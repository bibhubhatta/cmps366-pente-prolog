% Game state related predicates

:- module(game_state,
    [
        read_game_state/2,
        write_game_state/2,
        get_board/2,
        get_human_captures/2,
        get_human_tournament_score/2,
        get_computer_captures/2,
        get_computer_tournament_score/2,
        get_current_player/2,
        get_current_player_stone/2,
        get_stone_from_player/3,
        get_player_from_stone/3,
        other_stone/2,
        other_player/2,
        get_player_tournament_score/3,
        is_stone/1,
        get_player_captures/3,
        set_player_captures/4,
        set_player_tournament_score/4,
        set_current_player/3,
        set_current_player_stone/3,
        switch_player/2,
        switch_player_stone/2,
        switch_turn/2,
        get_initial_state/1,
        get_capture_sequence/2,
        handle_capture_in_direction/4,
        print_game_state/1,
        make_move/3,
        get_winner/2,
        is_game_drawn/1,
        is_game_over/1,
        get_round_score/3,
        initialize_round/2,
        update_tournament_score/2
    ]).

:- use_module(board).
:- use_module(position).



% Predicate to read game state from file
% The game state is a list
% [Board, HumanCaptures, HumanTournamentScore, ComputerCaptures, ComputerTournamentScore, CurrentPlayer, CurrentPlayerStone]
% Board is a list of lists, each list representing a row
% Each cell in the board is either o (empty), w (white) or b (black)
% https://www.swi-prolog.org/pldoc/man?section=isoIO
read_game_state(GameStateFile, GameState) :-
    open(GameStateFile, read, Stream),
    read(Stream, GameState),
    close(Stream).

% Predicate to write game state to file
% https://www.swi-prolog.org/pldoc/man?section=isoIO
write_game_state(GameStateFile, GameState) :-
    open(GameStateFile, write, Stream),
    write(Stream, GameState),
    % There should be a dot at the end so that the file can be read
    write(Stream, '.'),
    close(Stream).

% Predicate to get the board from the game state
% Assistance from: https://www.swi-prolog.org/pldoc/doc_for?object=nth0/3
% https://www.swi-prolog.org/pldoc/man?section=lists
get_board(GameState, Board) :-
    nth0(0, GameState, Board).

% Predicate to get the human captures from the game state
get_human_captures(GameState, HumanCaptures) :-
    nth0(1, GameState, HumanCaptures).

% Predicate to get the human tournament score from the game state
get_human_tournament_score(GameState, HumanTournamentScore) :-
    nth0(2, GameState, HumanTournamentScore).

% Predicate to get the computer captures from the game state
get_computer_captures(GameState, ComputerCaptures) :-
    nth0(3, GameState, ComputerCaptures).

% Predicate to get the computer tournament score from the game state
get_computer_tournament_score(GameState, ComputerTournamentScore) :-
    nth0(4, GameState, ComputerTournamentScore).

% Predicate to get the current player from the game state
get_current_player(GameState, CurrentPlayer) :-
    nth0(5, GameState, CurrentPlayer).

% Predicate to get the current player stone from the game state
% Assistance: https://www.swi-prolog.org/pldoc/man?predicate=atom_chars/2
get_current_player_stone(GameState, CurrentPlayerStone) :-
    % The stone is only the first letter of the stone name in the game state
    nth0(6, GameState, CurrentPlayerStoneString),
    atom_chars(CurrentPlayerStoneString, CurrentPlayerStoneChars),
    nth0(0, CurrentPlayerStoneChars, CurrentPlayerStone).

% Predicate to get the other player's stone
other_stone('w', 'b').
other_stone('b', 'w').
other_stone('black', 'white').
other_stone('white', 'black').

% Predicate to get the other player
other_player('human', 'computer').
other_player('computer', 'human').

% Predicate to get the stone that the player is playing
% https://www.cs.bham.ac.uk/research/projects/poplog/prolog/help/conditional
get_stone_from_player(GameState, Player, Stone) :-
    get_current_player(GameState, CurrentPlayer),
    get_current_player_stone(GameState, CurrentPlayerStone),
    Player = CurrentPlayer,
    Stone = CurrentPlayerStone.

get_stone_from_player(GameState, Player, Stone) :-
    get_current_player(GameState, CurrentPlayer),
    get_current_player_stone(GameState, CurrentPlayerStone),
    Player \= CurrentPlayer,
    other_stone(CurrentPlayerStone, Stone).

% Predicate to get the player that is playing the stone
get_player_from_stone(GameState, Stone, Player) :-
    get_current_player(GameState, CurrentPlayer),
    get_current_player_stone(GameState, CurrentPlayerStone),
    Stone = CurrentPlayerStone,
    Player = CurrentPlayer.
    
get_player_from_stone(GameState, Stone, Player) :-
    get_current_player(GameState, CurrentPlayer),
    get_current_player_stone(GameState, CurrentPlayerStone),
    Stone \= CurrentPlayerStone,
    other_player(CurrentPlayer, Player).


% Predicate to get the tournament score of a player
get_player_tournament_score(GameState, human, TournamentScore) :-
    get_human_tournament_score(GameState, TournamentScore).
get_player_tournament_score(GameState, computer, TournamentScore) :-
    get_computer_tournament_score(GameState, TournamentScore).

% Predicates to check if the stone is a valid stone
is_stone('w').
is_stone('b').
is_stone('white').
is_stone('black').

% Predicate to get the no. captures of a player
get_player_captures(GameState, human, Captures) :-
    get_human_captures(GameState, Captures).
get_player_captures(GameState, computer, Captures) :-
    get_computer_captures(GameState, Captures).

% Predicate to set the number of captures of the human player
set_human_captures([Board, _, HumanTournamentScore, ComputerCaptures, ComputerTournamentScore, CurrentPlayer, CurrentPlayerStone], Captures, [Board, Captures, HumanTournamentScore, ComputerCaptures, ComputerTournamentScore, CurrentPlayer, CurrentPlayerStone]).
% Predicate to set the number of captures of the computer player
set_computer_captures([Board, HumanCaptures, HumanTournamentScore, _, ComputerTournamentScore, CurrentPlayer, CurrentPlayerStone], Captures, [Board, HumanCaptures, HumanTournamentScore, Captures, ComputerTournamentScore, CurrentPlayer, CurrentPlayerStone]).

% Predicate to set the tournament score of the human player
set_human_tournament_score([Board, HumanCaptures, _, ComputerCaptures, ComputerTournamentScore, CurrentPlayer, CurrentPlayerStone], TournamentScore, [Board, HumanCaptures, TournamentScore, ComputerCaptures, ComputerTournamentScore, CurrentPlayer, CurrentPlayerStone]).
% Predicate to set the tournament score of the computer player
set_computer_tournament_score([Board, HumanCaptures, HumanTournamentScore, ComputerCaptures, _, CurrentPlayer, CurrentPlayerStone], TournamentScore, [Board, HumanCaptures, HumanTournamentScore, ComputerCaptures, TournamentScore, CurrentPlayer, CurrentPlayerStone]).


% Predicate to set the no. captures of a player
set_player_captures(GameState, human, Captures, NewGameState) :-
    set_human_captures(GameState, Captures, NewGameState).
set_player_captures(GameState, computer, Captures, NewGameState) :-
    set_computer_captures(GameState, Captures, NewGameState).

% Predicate to set the tournament score of a player
set_player_tournament_score(GameState, human, TournamentScore, NewGameState) :-
    set_human_tournament_score(GameState, TournamentScore, NewGameState).
set_player_tournament_score(GameState, computer, TournamentScore, NewGameState) :-
    set_computer_tournament_score(GameState, TournamentScore, NewGameState).

% update_tournament_score(+GameState, -NewGameState)
% Predicate to update the tournament score of the game state
update_tournament_score(GameState, NewGameState) :-
    get_round_score(GameState, human, HumanScore),
    get_round_score(GameState, computer, ComputerScore),
    get_player_tournament_score(GameState, human, HumanTournamentScore),
    get_player_tournament_score(GameState, computer, ComputerTournamentScore),
    NewHumanTournamentScore is HumanTournamentScore + HumanScore,
    NewComputerTournamentScore is ComputerTournamentScore + ComputerScore,
    set_player_tournament_score(GameState, human, NewHumanTournamentScore, GameState1),
    set_player_tournament_score(GameState1, computer, NewComputerTournamentScore, NewGameState).


% Predicate to set the current player
set_current_player([Board, HumanCaptures, HumanTournamentScore, ComputerCaptures, ComputerTournamentScore, _, CurrentPlayerStone], CurrentPlayer, [Board, HumanCaptures, HumanTournamentScore, ComputerCaptures, ComputerTournamentScore, CurrentPlayer, CurrentPlayerStone]).
% Predicate to set the current player stone
set_current_player_stone([Board, HumanCaptures, HumanTournamentScore, ComputerCaptures, ComputerTournamentScore, CurrentPlayer, _], CurrentPlayerStone, [Board, HumanCaptures, HumanTournamentScore, ComputerCaptures, ComputerTournamentScore, CurrentPlayer, CurrentPlayerStone]).

% set_board(+GameState, +Board, -NewGameState)
% Predicate to set the board of the game state
set_board([_, HumanCaptures, HumanTournamentScore, ComputerCaptures, ComputerTournamentScore, CurrentPlayer, CurrentPlayerStone], Board, [Board, HumanCaptures, HumanTournamentScore, ComputerCaptures, ComputerTournamentScore, CurrentPlayer, CurrentPlayerStone]).

% Predicate to switch the current player
switch_player(GameState, NewGameState) :-
    get_current_player(GameState, CurrentPlayer),
    other_player(CurrentPlayer, OtherPlayer),
    set_current_player(GameState, OtherPlayer, NewGameState).

% Predicate to switch the current player stone
switch_player_stone(GameState, NewGameState) :-
    get_current_player_stone(GameState, CurrentPlayerStone),
    other_stone(CurrentPlayerStone, OtherPlayerStone),
    set_current_player_stone(GameState, OtherPlayerStone, NewGameState).

% Predicate to switch the current turn
switch_turn(GameState, NewGameState) :-
    switch_player(GameState, SwitchedPlayerGameState),
    switch_player_stone(SwitchedPlayerGameState, NewGameState).

% Predicate to get the initial game state
% The initial game state is a list
% [Board, HumanCaptures, HumanTournamentScore, ComputerCaptures, ComputerTournamentScore, CurrentPlayer, CurrentPlayerStone]
get_initial_state(GameState) :-
    get_empty_board(19, 19, Board),
    GameState = [Board, 0, 0, 0, 0, '_', 'white'].

% Predicate to get the capture sequence of a stone
get_capture_sequence(Stone, CaptureSequence) :-
    other_stone(Stone, OtherStone),
    CaptureSequence = [Stone, OtherStone, OtherStone, Stone].

% handle_capture_in_direction(+GameState, +Move, +DirectionFunction, -NewGameState)
% Predicate to handle the capture in a direction
handle_capture_in_direction(GameState, Move, DirectionFunction, NewGameState) :-
    get_board(GameState, Board),
    get_current_player_stone(GameState, CurrentPlayerStone),
    get_capture_sequence(CurrentPlayerStone, CaptureSequence),
    % Get the cells in the direction
    call(DirectionFunction, Move, OneAwayCell),
    call(DirectionFunction, OneAwayCell, TwoAwayCell),
    call(DirectionFunction, TwoAwayCell, ThreeAwayCell),
    % Check if the cells have the capture sequence
    get_stone(Board, Move, ZeroAwayCellStone),
    get_stone(Board, OneAwayCell, OneAwayCellStone),
    get_stone(Board, TwoAwayCell, TwoAwayCellStone),
    get_stone(Board, ThreeAwayCell, ThreeAwayCellStone),
    Sequence = [ZeroAwayCellStone, OneAwayCellStone, TwoAwayCellStone, ThreeAwayCellStone],
    Sequence = CaptureSequence,
    % % Update the board
    set_stone(Board, OneAwayCell, 'o', NewBoard),
    set_stone(NewBoard, TwoAwayCell, 'o', NewNewBoard),
    set_board(GameState, NewNewBoard, BoardUpdatedGameState),
    % % Calculate the new captures
    get_player_from_stone(GameState, ZeroAwayCellStone, Player),
    get_player_captures(GameState, Player, Captures),
    NewCaptures is Captures + 1,
    set_player_captures(BoardUpdatedGameState, Player, NewCaptures, NewGameState).

handle_capture_in_direction(GameState, _, _, NewGameState) :-
    % No capture happened
    NewGameState = GameState.

% make_move(+GameState, +Move, -NewGameState)
% Predicate to make a move
% There is only one solution for a valid move
% https://stackoverflow.com/questions/20935671/member-predicate
make_move(GameState, Move, NewGameState) :-
    get_board(GameState, Board),
    get_current_player_stone(GameState, CurrentPlayerStone),
    % Check if the move is valid
    get_available_moves(Board, AvailableMoves),
    nonvar(AvailableMoves), memberchk(Move, AvailableMoves),
    % Update the board
    set_stone(Board, Move, CurrentPlayerStone, NewBoard),
    set_board(GameState, NewBoard, BoardUpdatedGameState),
    % Check if there is a capture in all direction, go clockwise
    handle_capture_in_direction(BoardUpdatedGameState, Move, up_position, UpGameState),
    handle_capture_in_direction(UpGameState, Move, up_right_position, UpRightGameState),
    handle_capture_in_direction(UpRightGameState, Move, right_position, RightGameState),
    handle_capture_in_direction(RightGameState, Move, down_right_position, DownRightGameState),
    handle_capture_in_direction(DownRightGameState, Move, down_position, DownGameState),
    handle_capture_in_direction(DownGameState, Move, down_left_position, DownLeftGameState),
    handle_capture_in_direction(DownLeftGameState, Move, left_position, LeftGameState),
    handle_capture_in_direction(LeftGameState, Move, up_left_position, UpLeftGameState),
    % Switch the turn
    switch_turn(UpLeftGameState, NewGameState).


% get_winner(+GameState, -Winner)
% Predicate to get the winner of the game
% Returns 'human' or 'computer', and fails if the game is not over
get_winner(GameState, human):-
    is_winner(GameState, human).
get_winner(GameState, computer):-
    is_winner(GameState, computer).

% is_winner(+GameState, +Winner)
% Predicate to check if the winner is the player that captured 5 or more stones or has a sequence of 5 or more stones
% Returns true if the winner is the player that captured 5 or more stones or has a sequence of 5 or more stones, false otherwise
is_winner(GameState, Winner) :-
    is_capture_winner(GameState, Winner);
    is_sequence_winner(GameState, Winner).

% is_capture_winner(+GameState, +Winner)
% Predicate to check if the winner is the player that captured 5 or more stones
% Returns true if the winner is the player that captured 5 or more stones, false otherwise
is_capture_winner(GameState, Winner) :-
    get_player_captures(GameState, Winner, NoCaptures),
    NoCaptures >= 5.

% is_sequence_winner(+GameState, +Winner)
% Predicate to check if the winner is the player that has a sequence of 5 or more stones
% Returns true if the winner is the player that has a sequence of 5 or more stones, false otherwise
is_sequence_winner(GameState, Winner):-
    get_board(GameState, Board),
    get_stone_from_player(GameState, Winner, Stone),
    get_all_stone_sequences(Board, Stone, Sequences),
    include(length_greater_than_or_equal_to(5), Sequences, FiveOrMoreSequences),
    length(FiveOrMoreSequences, NumFiveOrMore),
    NumFiveOrMore >= 1.

% Predicate to check if the game is drawn
% Returns true if the game is drawn, false otherwise
is_game_drawn(GameState) :-
    get_board(GameState, Board),
    get_available_moves(Board, AvailableMoves),
    AvailableMoves = [].

% Predicate to check if the game is over
% Returns true if the game is over, false otherwise
% The game is over if there is a winner or if the game is drawn
is_game_over(GameState) :-
    get_winner(GameState, _);
    is_game_drawn(GameState).

% get_round_score(+GameState, +Player, -Score)
% Predicate to get the round score of a player
% The score is the sum of the sequence score and the number of captures
get_round_score(GameState, Player, Score) :-
    get_board(GameState, Board),
    get_stone_from_player(GameState, Player, Stone),
    get_sequence_score(Board, Stone, SequenceScore),
    get_player_captures(GameState, Player, NoCaptures),
    Score is SequenceScore + NoCaptures.

% initialize_round(+GameState, -NewGameState)
% Predicate to initialize the game state for a new round
initialize_round(GameState, NewGameState) :-
    set_player_captures(GameState, human, 0, GameState1),
    set_player_captures(GameState1, computer, 0, GameState2),
    get_empty_board(19, 19, EmptyBoard),
    set_board(GameState2, EmptyBoard, GameState3),
    set_current_player_stone(GameState3, white, GameState4),
    set_current_player(GameState4, '_', NewGameState).


% Predicate to print the game state
print_game_state(GameState) :-
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
    get_player_tournament_score(GameState, human, HumanScore),
    format('Human tournament score: ~w~n', [HumanScore]),
    get_player_tournament_score(GameState, computer, ComputerScore),
    format('Computer tournament score: ~w~n', [ComputerScore]),
    get_current_player(GameState, CurrentPlayer),
    format('Current player: ~w~n', [CurrentPlayer]),
    get_current_player_stone(GameState, CurrentStone),
    format('Current stone: ~w~n', [CurrentStone]),
    nl.
