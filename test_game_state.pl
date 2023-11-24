:- begin_tests(game_state).

% Consult game_state.pl
:- consult(game_state).

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
    assertion(GameState == GameState2).

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
    


:-end_tests(game_state).

:- run_tests(game_state).