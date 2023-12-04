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
        load_game_state_from_human_input/1,
        human_wants_to_save_and_quit/0,
        save_to_human_location/1
    ]).

:- use_module(game_state).
:- use_module(board).
:-use_module(strategy).

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
    format('Enter your move, e.g. A10, ask help (h): ~n', []),
    read_line_to_string(user_input, Input),
    handle_human_input(GameState, Input, HumanMove).

% handle_human_input(+GameState, +HumanInput, -HumanMove)
% Handles the human input for different cases.
handle_human_input(GameState, HumanInput, HumanMove) :-
    string_upper(HumanInput, MoveString),
    MoveString = "H",
    print_help(GameState),
    get_human_move(GameState, HumanMove).

handle_human_input(GameState, HumanInput, HumanMove) :-
    string_upper(HumanInput, MoveString),
    atom_string(Move, MoveString),
    is_available_move(GameState, Move), HumanMove = Move.

handle_human_input(GameState, _, HumanMove) :-
    format('Invalid move. Please try again. ~n', []),
    get_human_move(GameState, HumanMove).

% print_help(+GameState)
% Prints the optimal move and rationale.
print_help(GameState) :-
    get_best_move(GameState, Move),
    format('The best move is ~w.~n', [Move]),
    print_rationale(GameState, Move).

print_rationale(GameState, Move) :-
    get_move_rationale(GameState, Move, Rationale),
    format('Rationale: ~w~n', [Rationale]).

% is_available_move(+GameState, +Move)
% Checks if the move is available in the current game state.
is_available_move(GameState, Move) :-
    get_board(GameState, Board),
    get_available_moves(Board, AvailableMoves),
    member(Move, AvailableMoves).

% save_to_human_location(+GameState)
% Saves the game state to the user's location.
save_to_human_location(GameState) :-
    format('Enter the file name to save the game: ~n', []),
    read_line_to_string(user_input, FileName),
    catch(
        write_game_state(FileName, GameState),
        Error,
        (
            print_message(error, Error),
            format('Could not save file. Please try again.~n', []),
            save_to_human_location(GameState)
        )
    ).

% human_wins_toss
% True if human wins the toss, false otherwise.
human_wins_toss :-
    writeln('Both human and computer have the same score'),    
    writeln('Tossing a coin to see who goes first...'),
    writeln('Heads or tails? (h/t): '),
    read_line_to_string(user_input, HumanChoice),
    string_upper(HumanChoice, Choice),
    handle_human_choice(Choice).

% handle_human_choice(+Choice)
% Handles the human choice for different cases.
handle_human_choice(Choice) :-
    not(memberchk(Choice, ["H", "T"])),
    writeln('Invalid input. Please try again.'),
    human_wins_toss.

handle_human_choice(_) :-
    random(0, 2, RandomNum),
    handle_toss_result(RandomNum).

% handle_toss_result(+RandomNum)
% Handles the toss result for different cases.
handle_toss_result(0) :-
    writeln('You won the toss! You will be playing first as white.'),
    nl,
    nl,
    true.
handle_toss_result(1) :-
    writeln('You lost the toss! You will be playing second as black.'),
    nl,
    nl,
    false.

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

% human_wants_to_save_and_quit
% True if human wants to save and quit, false otherwise.
human_wants_to_save_and_quit :-
    ask_yes_no_question('Do you want to save and quit?', WantsToSaveAndQuit),
    WantsToSaveAndQuit.

% ask_yes_no_question(+Question, -Response)
% Asks the user a yes/no question and returns the response as a boolean.
% If the user enters an invalid input, it will ask the user to enter a new input.
% If the user enters a valid input, it will return the response as a boolean.
ask_yes_no_question(Question, Response) :-
    format('~w (y/n): ', [Question]),
    read_line_to_string(user_input, HumanChoice),
    string_upper(HumanChoice, Choice),
    handle_choice(Question, Choice, Response).

% handle_choice(+Choice)
% Handles the choice for different cases.
handle_choice(Question, Choice, Response) :-
    not(memberchk(Choice, ["Y", "N"])),
    writeln('Invalid input. Please try again.'),
    ask_yes_no_question(Question, Response).

handle_choice(_, "Y", Response) :-
    Response = true.
handle_choice(_, "N", Response) :-
    Response = false.

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