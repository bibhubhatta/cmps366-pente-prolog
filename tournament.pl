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
    handle_round_result(AfterRoundGameState).

% handle_round_result(+GameState)
% Predicate to handle the round result
handle_round_result(AfterRoundGameState) :-
    % If the game is over, update the tournament score and announce the tournament scores
    is_game_over(AfterRoundGameState),
    update_tournament_score(AfterRoundGameState, FinalGameState),
    announce_tournament_scores(FinalGameState),
    handle_human_wants_to_play_again(FinalGameState).

handle_round_result(AfterRoundGameState) :-
    not(is_game_over(AfterRoundGameState)).

% handle_human_wants_to_play_again(+GameState)
% Predicate to handle the condition when asking the human if they want to play again
handle_human_wants_to_play_again(AfterRoundGameState) :-
    human_wants_to_play_again,
    initialize_round(AfterRoundGameState, InitializedGameState),
    set_starting_player(InitializedGameState, NewGameState),
    play_tournament(NewGameState).

handle_human_wants_to_play_again(AfterRoundGameState) :-
    announce_tournament_result(AfterRoundGameState).



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
    (announce_winner(GameState) ; announce_draw(GameState)),
    nl,
    nl,
    writeln('------------------------------------------').


% announce_winner(+GameState)
% Predicate to announce the tournament winner
announce_winner(GameState) :-
    get_tournament_winner(GameState, Winner),
    format('The tournament winner is ~w!~n', [Winner]).

announce_draw(GameState) :- 
    get_player_tournament_score(GameState, human, HumanScore),
    get_player_tournament_score(GameState, computer, ComputerScore),
    HumanScore =:= ComputerScore,
    writeln('The tournament is a draw!').

% get_tournament_winner(+GameState, -Winner)
% Predicate to get the tournament winner
get_tournament_winner(GameState, human) :-
    get_player_tournament_score(GameState, human, HumanScore),
    get_player_tournament_score(GameState, computer, ComputerScore),
    HumanScore > ComputerScore.
get_tournament_winner(GameState, computer) :-
    get_player_tournament_score(GameState, human, HumanScore),
    get_player_tournament_score(GameState, computer, ComputerScore),
    HumanScore < ComputerScore.