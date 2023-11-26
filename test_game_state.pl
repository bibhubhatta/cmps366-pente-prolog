:- begin_tests(game_state).

:- use_module(game_state).
:- use_module(board).

test(read_game_state) :-
    read_game_state('serials/4.pl', GameState),
    get_human_captures(GameState, HumanCaptures),
    assertion(HumanCaptures == 3),
    get_human_tournament_score(GameState, HumanTournamentScore),
    assertion(HumanTournamentScore == 6),
    get_computer_captures(GameState, ComputerCaptures),
    assertion(ComputerCaptures == 0),
    get_computer_tournament_score(GameState, ComputerTournamentScore),
    assertion(ComputerTournamentScore == 2),
    get_current_player(GameState, CurrentPlayer),
    assertion(CurrentPlayer == computer),
    get_current_player_stone(GameState, CurrentPlayerStone),
    assertion(CurrentPlayerStone == w),
    get_board(GameState, Board),
    assertion(Board ==    [
     [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ],
     [ o, o, o, b, b, o, o, o, o, o, o, o, o, o, o, o, o, o, o ],
     [ o, o, o, b, o, b, o, o, o, o, o, o, o, o, o, o, o, o, o ],
     [ o, o, o, w, o, o, w, o, o, o, o, o, o, o, o, o, o, o, o ],
     [ o, o, o, o, o, b, o, o, o, o, o, o, o, o, o, o, o, o, o ],
     [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ],
     [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ],
     [ o, o, o, o, o, o, o, o, o, o, o, w, o, o, o, o, o, o, o ],
     [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ],
     [ o, o, o, o, o, o, o, o, o, w, o, o, o, o, o, o, o, o, o ],
     [ o, o, o, o, o, o, o, o, w, o, o, o, o, o, o, o, o, o, o ],
     [ o, o, o, o, o, o, o, w, o, o, o, o, o, o, o, o, o, o, o ],
     [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ],
     [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, b, o, o, o ],
     [ o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, b, o, o, o ],
     [ o, o, o, o, w, w, b, o, o, o, o, o, o, o, o, o, o, o, o ],
     [ o, o, o, w, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ],
     [ o, o, o, w, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ],
     [ o, o, o, b, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o ]
    ]).  

test(read_save_read_game_state) :-
    read_game_state('serials/4.pl', GameState),
    write_game_state('serials/4_test_read_save.pl', GameState),
    read_game_state('serials/4_test_read_save.pl', GameState2),
    assertion(GameState == GameState2),
    % Delete the test file
    delete_file('serials/4_test_read_save.pl').

test(other_stone) :-
    other_stone('w', OtherStone1),
    assertion(OtherStone1 == 'b'),
    other_stone('b', OtherStone2),
    assertion(OtherStone2 == 'w'),
    other_stone('white', OtherStone3),
    assertion(OtherStone3 == 'black'),
    other_stone('black', OtherStone4),
    assertion(OtherStone4 == 'white').


test(other_player) :-
    other_player('human', OtherPlayer1),
    assertion(OtherPlayer1 == 'computer'),
    other_player('computer', OtherPlayer2),
    assertion(OtherPlayer2 == 'human').

test(get_stone_from_player) :-
    GameState = ['_', '_', '_', '_', '_', 'human', 'w'],
    get_stone_from_player(GameState, 'human', Stone1),
    assertion(Stone1 == 'w'),
    get_stone_from_player(GameState, 'computer', Stone2),
    assertion(Stone2 == 'b'),
    read_game_state('serials/4.pl', GameState2),
    get_stone_from_player(GameState2, 'human', Stone3),
    assertion(Stone3 == 'b'),
    get_stone_from_player(GameState2, 'computer', Stone4),
    assertion(Stone4 == 'w').

test(get_player_from_stone) :-
    GameState = ['_', '_', '_', '_', '_', 'human', 'w'],
    assertion(get_player_from_stone(GameState, 'w', 'human')),
    assertion(get_player_from_stone(GameState, 'b', 'computer')).
    
test(get_player_tournament_score) :-
    read_game_state('serials/4.pl', GameState),
    get_player_tournament_score(GameState, 'human', HumanTournamentScore),
    assertion(HumanTournamentScore == 6),
    get_player_tournament_score(GameState, 'computer', ComputerTournamentScore),
    assertion(ComputerTournamentScore == 2).

test(is_stone) :-
    assertion(is_stone('w')),
    assertion(is_stone('b')),
    assertion(is_stone('white')),
    assertion(is_stone('black')),
    assertion(\+ is_stone('o')),
    assertion(\+ is_stone('human')),
    assertion(\+ is_stone('computer')),
    assertion(\+ is_stone('human')).

test(get_player_captures) :-
    read_game_state('serials/4.pl', GameState),
    get_player_captures(GameState, 'human', HumanCaptures),
    assertion(HumanCaptures == 3),
    get_player_captures(GameState, 'computer', ComputerCaptures),
    assertion(ComputerCaptures == 0).

test(set_player_captures) :-
    read_game_state('serials/4.pl', GameState),
    set_player_captures(GameState, 'human', 5, NewGameState),
    get_player_captures(NewGameState, 'human', HumanCaptures),
    assertion(HumanCaptures == 5),
    get_player_captures(NewGameState, 'computer', ComputerCaptures),
    assertion(ComputerCaptures == 0),
    set_player_captures(GameState, 'computer', 5, NewGameState2),
    get_player_captures(NewGameState2, 'computer', ComputerCaptures2),
    assertion(ComputerCaptures2 == 5).

test(set_player_tournament_score) :-
    read_game_state('serials/4.pl', GameState),
    set_player_tournament_score(GameState, 'human', 5, NewGameState),
    get_player_tournament_score(NewGameState, 'human', HumanTournamentScore),
    assertion(HumanTournamentScore == 5),
    get_player_tournament_score(NewGameState, 'computer', ComputerTournamentScore),
    assertion(ComputerTournamentScore == 2),
    set_player_tournament_score(GameState, 'computer', 5, NewGameState2),
    get_player_tournament_score(NewGameState2, 'computer', ComputerTournamentScore2),
    assertion(ComputerTournamentScore2 == 5).

test(set_current_player) :-
    read_game_state('serials/4.pl', GameState),
    set_current_player(GameState, 'human', NewGameState),
    get_current_player(NewGameState, CurrentPlayer),
    assertion(CurrentPlayer == 'human'),
    set_current_player(GameState, 'computer', NewGameState2),
    get_current_player(NewGameState2, CurrentPlayer2),
    assertion(CurrentPlayer2 == 'computer').

test(set_current_player_stone) :-
    read_game_state('serials/4.pl', GameState),
    set_current_player_stone(GameState, 'w', NewGameState),
    get_current_player_stone(NewGameState, CurrentPlayerStone),
    assertion(CurrentPlayerStone == 'w'),
    set_current_player_stone(GameState, 'b', NewGameState2),
    get_current_player_stone(NewGameState2, CurrentPlayerStone2),
    assertion(CurrentPlayerStone2 == 'b').

test(switch_turn) :-
    read_game_state('serials/4.pl', GameState),
    get_current_player(GameState, CurrentPlayer),
    assertion(CurrentPlayer == 'computer'),
    get_current_player_stone(GameState, CurrentPlayerStone),
    assertion(CurrentPlayerStone == 'w'),
    switch_turn(GameState, NewGameState),
    get_current_player(NewGameState, NewCurrentPlayer),
    assertion(NewCurrentPlayer == 'human'),
    get_current_player_stone(NewGameState, NewCurrentPlayerStone),
    assertion(NewCurrentPlayerStone == 'b').

test(get_initial_state) :-
    get_initial_state(GameState),
    get_player_captures(GameState, 'human', HumanCaptures),
    assertion(HumanCaptures == 0),
    get_player_captures(GameState, 'computer', ComputerCaptures),
    assertion(ComputerCaptures == 0),
    get_player_tournament_score(GameState, 'human', HumanTournamentScore),
    assertion(HumanTournamentScore == 0),
    get_player_tournament_score(GameState, 'computer', ComputerTournamentScore),
    assertion(ComputerTournamentScore == 0),
    get_current_player(GameState, CurrentPlayer),
    % The current player should not be assigned in the initial state
    assertion(CurrentPlayer == '_'),
    get_current_player_stone(GameState, CurrentPlayerStone),
    assertion(CurrentPlayerStone == 'w'),
    get_board(GameState, Board),
    assertion(Board == [[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o],[o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o]]).

test(get_capture_sequence) :-
    get_capture_sequence('w', WCaptureSequence),
    assertion(WCaptureSequence == [w, b, b, w]),
    get_capture_sequence('b', BCaptureSequence),
    assertion(BCaptureSequence == [b, w, w, b]).

test(handle_capture) :-
    read_game_state('serials/capture_test.pl', GameState),
    CapturedPositions = ['J11', 'J12', 'K11', 'L12', 'K10', 'L10', 'K9', 'L8', 'J9', 'J8', 'I9', 'H8', 'I10', 'H10', 'H12', 'I11'],
    get_board(GameState, BoardBefore),
    % Find cells occupied by black
    % get_all_positions(BoardBefore, AllPositions),
    % findall(Position, (member(Position, AllPositions), get_stone(BoardBefore, Position, 'b')), BlackPositions),
    get_stones(BoardBefore, CapturedPositions, CapturedPositionStonesBefore),
    sort(CapturedPositionStonesBefore, CapturedPositionStonesBeforeSorted),
    assertion(CapturedPositionStonesBeforeSorted == ['b']),
    make_move(GameState, 'J10', NewGameState),
    get_board(NewGameState, BoardAfter),
    get_stones(BoardAfter, CapturedPositions, CapturedPositionStonesAfter),
    sort(CapturedPositionStonesAfter, CapturedPositionStonesAfterSorted),
    assertion(CapturedPositionStonesAfterSorted == ['o']).

test(handle_capture_when_no_capture) :-
    get_initial_state(InitialGameState),
    set_current_player(InitialGameState, 'human', GameState),
    make_move(GameState, 'J10', _).

test(handle_capture_)



:-end_tests(game_state).

:- run_tests(game_state).