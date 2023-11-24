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

:-end_tests(game_state).

:- run_tests(game_state).