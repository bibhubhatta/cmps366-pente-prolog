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
    (Player = CurrentPlayer -> Stone = CurrentPlayerStone ; other_stone(CurrentPlayerStone, Stone)).

% Predicate to get the player that is playing the stone
get_player_from_stone(GameState, Stone, Player) :-
    get_current_player(GameState, CurrentPlayer),
    get_current_player_stone(GameState, CurrentPlayerStone),
    (Stone = CurrentPlayerStone -> Player = CurrentPlayer ; other_player(CurrentPlayer, Player)).

