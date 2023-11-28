:- module(human,
    [
        human_wins_toss/0,
        human_wants_to_play_again/0,
        human_wants_to_load_game/0,
        get_human_move/2,
        ask_yes_no_question/2,
        human_wants_to_play_again/0,
        human_wins_toss/0,
        human_wants_to_load_game/0,
        ask_save_game/1,
        load_game_state_from_human_input/1
    ]).

:- use_module(game_state).
:- use_module(board).

% print_help
% print_help(GameState) :-
%     get_best_move_optimized(GameState, Move),
%     format('Optimal move: ~w~n', [Move]),
%     get_move_rationale(GameState, Move, Rationale),
%     get_explanation_from_rationales(Rationale, Explanation),
%     format('Rationale: ~w~n', [Explanation]).

% get_human_move(+GameState, -HumanMove)
% Gets the human move from the user and checks if it is valid.
% If the move is invalid, it will ask the user to enter a new move.
% If the move is valid, it will return the move as an atom.
% If the user wants to quit, it will save the game and quit.
% If the user wants help, it will print the optimal move and rationale.
get_human_move(GameState, HumanMove) :-
    format('Enter your move, e.g. A10, ask help (h), or quit (q): ~n', []),
    read_line_to_string(user_input, Input),
    string_upper(Input, MoveString),
    (MoveString = "Q" -> save_and_quit(GameState);
    % MoveString = "H" -> print_help(GameState), get_human_move(GameState, Move);
    atom_string(Move, MoveString),
    is_available_move(GameState, Move) -> HumanMove = Move;
    format('Invalid move. Please try again. ~n', []), get_human_move(GameState, HumanMove)
    ).

% is_available_move(+GameState, +Move)
% Checks if the move is available in the current game state.
is_available_move(GameState, Move) :-
    get_board(GameState, Board),
    get_available_moves(Board, AvailableMoves),
    member(Move, AvailableMoves).

% save_and_quit(+GameState)
% Saves the game state to the user's location and quits the game.
save_and_quit(GameState) :-
    format('Enter the file name to save the game: ~n', []),
    read_line_to_string(user_input, FileName),
    write_game_state(FileName, GameState),
    halt.

% human_wins_toss
% True if human wins the toss, false otherwise.
human_wins_toss :-
    writeln('Both human and computer have the same score'),    
    writeln('Tossing a coin to see who goes first...'),
    writeln('Heads or tails? (h/t): '),
    read_line_to_string(user_input, HumanChoice),
    string_upper(HumanChoice, Choice),
    random(0, 2, CoinToss),
    (not(member(Choice, ["H", "T"])) ->
        writeln('Invalid input. Please try again.'),
        human_wins_toss;
    CoinToss = 1 ->
        writeln('You won the toss! You will be playing the first turn as white.'),
        true;
    CoinToss = 0 ->
        writeln('You lost the toss! You will be playing the second turn as black.'),
        false
    ).

% human_wants_to_play_again
% True if human wants to play again, false otherwise.
human_wants_to_play_again :-
    ask_yes_no_question('Do you want to play again?', WantsToPlayAgain),
    WantsToPlayAgain.

% human_wants_to_load_game
% True if human wants to load game, false otherwise.
human_wants_to_load_game :-
    ask_yes_no_question('Do you want to load from file?', WantsToLoad),
    WantsToLoad.

% ask_yes_no_question(+Question, -Response)
% Asks the user a yes/no question and returns the response as a boolean.
% If the user enters an invalid input, it will ask the user to enter a new input.
% If the user enters a valid input, it will return the response as a boolean.
ask_yes_no_question(Question, Response) :-
    format('~w (y/n): ', [Question]),
    read_line_to_string(user_input, HumanChoice),
    string_upper(HumanChoice, Choice),
    (not(member(Choice, ["Y", "N"])) ->
        write('Invalid input. Please try again.'),
        nl,
        ask_yes_no_question(Question, Response);
    Choice = "Y" ->
        Response = true;
    Choice = "N" ->
        Response = false
    ).

% ask_save_game(+GameState, -FinalGameState)
% Asks the user if they want to save the game and quit.
% If the user wants to save the game, it will save the game and quit.
% Otherwise it will do nothing
ask_save_game(GameState) :-
    ask_yes_no_question('Do you want to save the game?', WantsToSave),
    (WantsToSave -> save_and_quit(GameState); true).


% load_game_state_from_human_input(-GameState)
% Asks the user for the file name to load the game from.
% If the file name is invalid, it will ask the user to enter a new file name.
% If the file name is valid, it will return the game state.
% https://eu.swi-prolog.org/pldoc/man?section=exception
load_game_state_from_human_input(GameState) :-
    format('Enter the file name to load the game: ~n', []),
    read_line_to_string(user_input, FileName),
    (
        catch(
            read_game_state(FileName, GameState),
            Error,
            (
                print_message(error, Error),
                format('Could not load file. Please try again.~n', []),
                load_game_state_from_human_input(GameState)
            )
        )
    ).