:- module(tournament, 
    [play_tournament/1]
    ).

:- use_module(round).
:- use_module(game_state).
:- use_module(human).


% play_tournament(+GameState)
% Predicate to play the tournament
play_tournament(GameState) :-
    announce_tournament_scores(GameState),
    conduct_round(GameState, AfterRoundGameState),
    update_tournament_score(AfterRoundGameState, FinalGameState),
    announce_tournament_scores(FinalGameState),
    (
        human_wants_to_play_again ->  
            (
                initialize_round(FinalGameState, InitializedGameState), 
                set_starting_player(InitializedGameState, NewGameState),
                play_tournament(NewGameState)
            );
            announce_tournament_result(FinalGameState),
            halt
    ).



% announce_tournament_scores(+GameState)
% Predicate to announce the tournament scores
announce_tournament_scores(GameState) :-
    writeln('--------- Tournament Scores ----------'),
    get_player_tournament_score(GameState, human, HumanScore),
    format('Human: ~w~n', [HumanScore]),
    get_player_tournament_score(GameState, computer, ComputerScore),
    format('Computer: ~w~n', [ComputerScore]),
    writeln('--------------------------------------'),
    nl.

% announce_tournament_result(+GameState)
% Predicate to announce the tournament result
announce_tournament_result(GameState) :-
    nl,
    nl,
    writeln('------------ Tournament Result -----------'),
    nl,
    nl,
    announce_tournament_scores(GameState),
    nl,
    (get_tournament_winner(GameState, Winner) ->  format('The tournament winner is ~w!~n', [Winner]);
    writeln('The tournament is a tie!')),
    writeln('------------------------------------------').

% get_tournament_winner(+GameState, -Winner)
% Predicate to get the tournament winner
get_tournament_winner(GameState, Winner) :-
    get_player_tournament_score(GameState, human, HumanScore),
    get_player_tournament_score(GameState, computer, ComputerScore),
    (
    HumanScore > ComputerScore ->  Winner = human;
    ComputerScore > HumanScore ->  Winner = computer;
    fail
    ).