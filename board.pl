:- module(board, [
    get_empty_board/3,
    get_row/3,
    get_col/3,
    get_char_list/2,
    mark_columns/2,
    mark_rows/2,
    cartesian_board/2,
    print_board/1,
    get_stone/3,
    get_stones/3,
    set_stone/4,
    convert_to_sequences/2,
    get_empty_positions/2,
    get_no_stones_on_board/2,
    get_all_board_columns/2,
    get_neighboring_stones/3,
    get_up_right_diagonal/3,
    get_up_left_diagonal/3,
    get_down_right_diagonal/3,
    get_down_left_diagonal/3,
    get_positive_diagonal/3,
    get_negative_diagonal/3,
    get_all_positive_diagonal_starts/2,
    get_all_negative_diagonal_starts/2,
    get_first_row_positions/2,
    get_first_column_positions/2,
    get_last_row_positions/2,
    get_all_positive_diagonals/2,
    get_all_negative_diagonals/2,
    get_all_diagonals/2,
    get_all_board_sequences/2,
    convert_board_sequences_to_stone_sequences/3,
    get_all_stone_sequences/3,
    contains_stone/2,
    get_board_size/3,
    get_center/2,
    get_positions_3_away_from_center/2,
    is_first_move/1,
    is_third_move/1,
    get_available_moves/2,
    get_all_positions/2,
    get_sequence_score/3,
    valid_position/2,
    length_greater_than_or_equal_to/2
]).

:- use_module(library(lists)).
:- use_module(position).

% Predicate to get the size of the board
% get_board_size(+Board, -NoRows, -NoCols)
get_board_size(Board, NoRows, NoCols) :-
    length(Board, NoRows),
    nth0(0, Board, FirstRow),
    length(FirstRow, NoCols).

% Predicate to get an empty board
% get_empty_board(+NoRows, +NoCols, -Board)
% https://www.swi-prolog.org/pldoc/doc_for?object=length/2
% https://www.swi-prolog.org/pldoc/man?predicate=maplist/2
get_empty_board(NoRows, NoCols, Board) :-
    length(Board, NoRows),
    maplist(get_empty_row(NoCols), Board).

% Predicate to get an empty row
% get_empty_row(+NoCols, -Row)
% All the elements of the row are o
get_empty_row(NoCols, Row) :-
    length(Row, NoCols),
    maplist(=('o'), Row).

% Predicate to get a row of the board
% get_row(+Board, +RowNum, -Row)
% The RowNum is the number displayed on the left of the board, and not the index of the row
get_row(Board, RowNum, Row) :-
    RowIndex is 19 - RowNum,
    nth0(RowIndex, Board, Row).

% Predicate to get a column of the board
% get_col(+Board, +ColChar, -Col)
% The ColChar is the letter displayed on the bottom of the board
get_col(Board, ColChar, Col) :-
    % Convert ColChar to uppercase to get consistent results
    upcase_atom(ColChar, ColCharUpper),
    % Get the index of the column
    char_code(ColCharUpper, ColCode),
    ColIndex is ColCode - 65,
    % Get the column
    maplist(nth0(ColIndex), Board, Col).


% Predicate to get a list of characters for the bottom of the board
% get_char_list(+NoCols, -CharList)
% Assistance: https://stackoverflow.com/questions/40711908/what-is-a-test-succeeded-with-choicepoint-warning-in-pl-unit-and-how-do-i-fix
get_char_list(NoCols, CharList) :-
    % Base case
    % get_char_list(0, []) is not used because it would make it non-deterministic
    % even if it is deterministic
    NoCols =:= 0 -> CharList = [];
    NoCols > 0,
    NoCols1 is NoCols - 1,
    get_char_list(NoCols1, CharList1),
    CharCode is NoCols + 64,
    char_code(Char, CharCode),
    append(CharList1, [Char], CharList).
    

% Predicate to mark the columns of a board
% mark_columns(+Board, -MarkedBoard)
mark_columns(Board, NewBoard) :-
    nth0(0, Board, FirstRow),
    length(FirstRow, NoCols),
    get_char_list(NoCols, CharList),
    append(Board, [CharList], NewBoard).

% Predicate to mark the rows of a board
% mark_rows(+Board, -MarkedBoard)
mark_rows([], []).
mark_rows([Head|Tail], [[N|Head]|MarkedTail]) :-
    length([Head|Tail], N),
    mark_rows(Tail, MarkedTail).

% cartesian_board(+UnmarkedBoard, -MarkedBoard)
% Purpose: Get board with position indicators for each row and column
% Return : The board -- a list of lists
cartesian_board(UnmarkedBoard, MarkedBoard) :-
    mark_columns(UnmarkedBoard, MarkedColumnsBoard),
    mark_rows(UnmarkedBoard, MarkedRowsBoard),
    % Extract the last row of the marked columns board
    length(MarkedColumnsBoard, NoRows),
    nth1(NoRows, MarkedColumnsBoard, LastRow),
    % Add a a blank space in the beginning of the last row
    append([' '], LastRow, LastRowWithSpace),
    % Append the last row to the marked rows board
    append(MarkedRowsBoard, [LastRowWithSpace], MarkedBoard).
    
% print_board_cell(+Cell)
% Purpose: Print a cell of the board
% https://apps.nms.kcl.ac.uk/reactome-pengine/documentation/man?section=format
print_board_cell(Cell) :-
    % if the cell is a number, it is a row marker, so print it as two digits for alignment
    (   number(Cell) -> format('~|~` t~d~2+', Cell)
    ;   % if the cell is ' ', it is the beginning of a row marker, so print two spaces for alignment
        (   Cell = ' ' -> write('  ')
        ;   % otherwise, print the character
            write(Cell)
        )
    ),
    % print a space for readability
    write(' ').

% print_row(+Row)
% Purpose: Print a row of the board
print_row([]) :-
    % print a newline to separate rows
    nl.
print_row([Head|Tail]) :-
    print_board_cell(Head),
    print_row(Tail).

% print_board(+Board)
% Purpose: Print the board
print_board([]).
print_board([Head|Tail]) :-
    print_row(Head),
    print_board(Tail).

% get_stone(+Board, +Position, -Stone)
% Purpose: Get the stone at a position on the board
get_stone(Board, Position, Stone) :-
    string_to_position(Position, RowIndex, ColIndex),
    nth0(RowIndex, Board, Row),
    nth0(ColIndex, Row, Stone).

% get_stones(+Board, +Positions, -Stones)
% Purpose: Get the stones at a list of positions on the board
% Using once to prevent multiple solutions even though it is deterministic
% https://www.swi-prolog.org/pldoc/man?predicate=once/1
get_stones(_, [], []).
get_stones(Board, [Head|Tail], [Stone|Stones]) :-
    get_stone(Board, Head, Stone),
    once(get_stones(Board, Tail, Stones)).

% set_stone(+Board, +Position, +Stone, -NewBoard)
% Purpose: Set the stone at a position on the board
% Note: Uses cut (!), otherwise the Prolog interpreter will think it has multiple solutions
% even though it is deterministic. This makes it cleaner to test and use.
set_stone(Board, Position, black, NewBoard) :-
    set_stone(Board, Position, b, NewBoard), !.

set_stone(Board, Position, white, NewBoard) :-
    set_stone(Board, Position, w, NewBoard), !.

set_stone(Board, Position, empty, NewBoard) :-
    set_stone(Board, Position, o, NewBoard), !.

set_stone(Board, Position, Stone, NewBoard) :-
    string_to_position(Position, RowIndex, ColIndex),
    get_board_size(Board, NoRows, NoCols),
    % Check if the position is valid
    RowIndex >= 0,
    RowIndex < NoRows,
    ColIndex >= 0,
    ColIndex < NoCols,
    % Get the row
    nth0(RowIndex, Board, Row),
    % Replace the stone
    replace(Row, ColIndex, Stone, NewRow),
    % Replace the row
    replace(Board, RowIndex, NewRow, NewBoard).

% replace(+List, +Index, +Element, -NewList)
% Purpose: Replace an element in a list
% Note: Uses cut (!), otherwise the Prolog interpreter will think it has multiple solutions
% even though it is deterministic. This makes it cleaner to test and use.
replace([_|Tail], 0, Element, [Element|Tail]) :- !.
replace([Head|Tail], Index, Element, [Head|NewTail]) :-
    Index > 0,
    Index1 is Index - 1,
    replace(Tail, Index1, Element, NewTail)
    % .
    , !.


% convert_to_sequences(+List, -Sequences)
% Purpose: Convert the list of stones to a list of sequences
% A sequence is a list of stones of the same color
% The list of sequences is a list of lists of stones
% Code was initially written without cut, but it makes the interpreter think it has multiple solutions
% even though it is deterministic. This makes it cleaner to test and use.
convert_to_sequences([], [[]]).
convert_to_sequences([X], [[X]]).
convert_to_sequences([X, X|Tail], [[X|SubSeq]|Sequences]) :-
    convert_to_sequences([X|Tail], [SubSeq|Sequences])
    % .
    , !.

convert_to_sequences([X, Y|Tail], [[X]|Sequences]) :-
    dif(X, Y),
    convert_to_sequences([Y|Tail], Sequences)
    % .
    , !.

% get_all_positions(+Board, -Positions)
% Purpose: Get all the positions on the board
% The position is a string of the form 'A1'
% https://www.swi-prolog.org/pldoc/man?predicate=findall/3
% https://www.swi-prolog.org/pldoc/man?predicate=bagof/3
% https://www.cse.unsw.edu.au/~billw/dictionaries/prolog/findall.html
get_all_positions(Board, Positions) :-
    get_board_size(Board, NoRows, NoCols),
    MaxRowIndex is NoRows - 1,
    MaxColIndex is NoCols - 1,
    findall(Position,
            (between(0, MaxRowIndex, RowIndex), 
            between(0, MaxColIndex, ColIndex),
            position_to_string(RowIndex, ColIndex, NoRows, Position)), 
            Positions).

% get_empty_positions(+Board, -Positions)
% Purpose: Get the empty positions on the board
get_empty_positions(Board, Positions) :-
    get_all_positions(Board, AllPositions),
    findall(Position,
            (member(Position, AllPositions),
            get_stone(Board, Position, 'o')),
            Positions).

% get_no_stones_on_board(+Board, -NoStones)
% Purpose: Get the number of stones on the board
% https://www.swi-prolog.org/pldoc/doc_for?object=flatten/2
% https://www.swi-prolog.org/pldoc/man?predicate=include/3
get_no_stones_on_board(Board, NoStones) :-
    % Unravel Board
    flatten(Board, FlatBoard),
    % Count the number of stones
    include(is_stone, FlatBoard, Stones),
    length(Stones, NoStones).

% get_no_stones_on_board(+Board, +Stone, -NoStones)
% Purpose: Get the number of stones of a specific color on the board
get_no_stones_on_board(Board, Stone, NoStones) :-
    % Unravel Board
    flatten(Board, FlatBoard),
    % Count the number of stones
    include(=(Stone), FlatBoard, Stones),
    length(Stones, NoStones).

get_no_stones_on_board(Board, 'white', NoStones) :-
    get_no_stones_on_board(Board, 'w', NoStones).
get_no_stones_on_board(Board, 'black', NoStones) :-
    get_no_stones_on_board(Board, 'b', NoStones).
get_no_stones_on_board(Board, 'empty', NoStones) :-
    get_no_stones_on_board(Board, 'o', NoStones).

% is_stone(+Stone)
% Purpose: Check if the given atom is a stone
is_stone(Stone) :-
    Stone = 'b' ; Stone = 'w'; Stone = 'black'; Stone = 'white'.


% get_all_board_columns(+Board, -Columns)
% Purpose: Get all the columns of the board
% The columns are a list of lists of stones
get_all_board_columns(Board, Columns) :-
    get_board_size(Board, _, NoCols),
    get_char_list(NoCols, CharList),
    maplist(get_col(Board), CharList, Columns).


% get_neighboring_stones(+Board, +PositionString, -NeighboringStones)
% Predicate to get all the neighboring stones of the given position
% NeighboringStones is a list of all the neighboring stones of the given position in clockwise order starting from the top
% The position is a string of the form 'A1'
% The neighboring stones are a list of atoms
get_neighboring_stones(Board, PositionString, NeighboringStones) :-
    get_neighbors(PositionString, Neighbors),
    get_stones(Board, Neighbors, NeighboringStonesWithEmpty),
    % Filter out empty stones
    include(is_stone, NeighboringStonesWithEmpty, NeighboringStones).

% get_up_right_diagonal(+Board, +PositionString, -UpRightDiagonal)
% Predicate to get all the stones on the up right diagonal of the given position
% UpRightDiagonal is a list of all the stones on the up right diagonal of the given position
% The position is a string of the form 'A1'
% The up right diagonal is a list of atoms
get_up_right_diagonal(Board, PositionString, UpRightDiagonal) :-
    up_right_position(PositionString, NewPositionString),
    (valid_position(NewPositionString) ->
        get_stone(Board, NewPositionString, Stone),
        get_up_right_diagonal(Board, NewPositionString, RestOfDiagonal),
        UpRightDiagonal = [Stone|RestOfDiagonal]
    ;   UpRightDiagonal = []
    ).

% get_up_left_diagonal(+Board, +PositionString, -UpLeftDiagonal)
% Predicate to get all the stones on the up left diagonal of the given position
% UpLeftDiagonal is a list of all the stones on the up left diagonal of the given position
% The position is a string of the form 'A1'
% The up left diagonal is a list of atoms
get_up_left_diagonal(Board, PositionString, UpLeftDiagonal) :-
    up_left_position(PositionString, NewPositionString),
    (valid_position(NewPositionString) ->
        get_stone(Board, NewPositionString, Stone),
        get_up_left_diagonal(Board, NewPositionString, RestOfDiagonal),
        UpLeftDiagonal = [Stone|RestOfDiagonal]
    ;   UpLeftDiagonal = []
    ).

% get_down_right_diagonal(+Board, +PositionString, -DownRightDiagonal)
% Predicate to get all the stones on the down right diagonal of the given position
% DownRightDiagonal is a list of all the stones on the down right diagonal of the given position
% The position is a string of the form 'A1'
% The down right diagonal is a list of atoms
get_down_right_diagonal(Board, PositionString, DownRightDiagonal) :-
    down_right_position(PositionString, NewPositionString),
    (valid_position(NewPositionString) ->
        get_stone(Board, NewPositionString, Stone),
        get_down_right_diagonal(Board, NewPositionString, RestOfDiagonal),
        DownRightDiagonal = [Stone|RestOfDiagonal]
    ;   DownRightDiagonal = []
    ).

% get_down_left_diagonal(+Board, +PositionString, -DownLeftDiagonal)
% Predicate to get all the stones on the down left diagonal of the given position
% DownLeftDiagonal is a list of all the stones on the down left diagonal of the given position
% The position is a string of the form 'A1'
% The down left diagonal is a list of atoms
get_down_left_diagonal(Board, PositionString, DownLeftDiagonal) :-
    down_left_position(PositionString, NewPositionString),
    (valid_position(NewPositionString) ->
        get_stone(Board, NewPositionString, Stone),
        get_down_left_diagonal(Board, NewPositionString, RestOfDiagonal),
        DownLeftDiagonal = [Stone|RestOfDiagonal]
    ;   DownLeftDiagonal = []
    ).

% get_positive_diagonal(+Board, +PositionString, -PositiveDiagonal)
% Predicate to get all the stones on the positive diagonal of the given position
% PositiveDiagonal is a list of all the stones on the positive diagonal of the given position
% The position is a string of the form 'A1'
% The positive diagonal is a list of atoms
% https://www.swi-prolog.org/pldoc/man?predicate=reverse/2
get_positive_diagonal(Board, PositionString, PositiveDiagonal) :-
    get_down_left_diagonal(Board, PositionString, DownLeftDiagonal),
    % Reverse the down left diagonal to get the diagonal in the correct order
    reverse(DownLeftDiagonal, DownLeftReversed),
    get_stone(Board, PositionString, Stone),
    get_up_right_diagonal(Board, PositionString, UpRightDiagonal),
    append(DownLeftReversed, [Stone|UpRightDiagonal], PositiveDiagonal).
    
% get_negative_diagonal(+Board, +PositionString, -NegativeDiagonal)
% Predicate to get all the stones on the negative diagonal of the given position
% NegativeDiagonal is a list of all the stones on the negative diagonal of the given position
% The position is a string of the form 'A1'
% The negative diagonal is a list of atoms
get_negative_diagonal(Board, PositionString, NegativeDiagonal) :-
    get_up_left_diagonal(Board, PositionString, UpLeftDiagonal),
    % Reverse the up left diagonal to get the diagonal in the correct order
    reverse(UpLeftDiagonal, UpLeftReversed),
    get_stone(Board, PositionString, Stone),
    get_down_right_diagonal(Board, PositionString, DownRightDiagonal),
    append(UpLeftReversed, [Stone|DownRightDiagonal], NegativeDiagonal).



% get_first_column_positions(+Board, -Positions)
% Predicate to get all the positions of the first column
% Positions is a list of position strings
get_first_column_positions(Board, Positions) :-
    get_board_size(Board, NoRows, NoCols),
    % Get the range of the column numbers
    MaxColIndex is NoCols - 1,
    findall(N, between(0, MaxColIndex, N), RowNumbers),
    % Generate a position string for each row number with position_to_string
    findall(Position, (member(RowNumber, RowNumbers), position_to_string(RowNumber, 0, NoRows, Position)), Positions).

% get_first_row_positions(+Board, -Positions)
% Predicate to get all the positions of the first row
% Positions is a list of position strings
get_first_row_positions(Board, FirsRowPositions) :-
    get_board_size(Board, NoRows, NoCols),
    % Get the range of the row numbers
    FirstRowIndex is NoRows - 1,
    MaxColIndex is NoCols - 1,
    findall(N, between(0, MaxColIndex, N), ColNumbers),
    % Generate a position string for each column number with position_to_string
    findall(Position, (member(ColNumber, ColNumbers), position_to_string(FirstRowIndex, ColNumber, NoRows, Position)), Positions),
    sort(Positions, FirsRowPositions).


% get_last_row_positions(+Board, -Positions)
% Predicate to get all the positions of the last row
% Positions is a list of position strings
get_last_row_positions(Board, Positions) :-
    get_board_size(Board, NoRows, NoCols),
    % Get the range of the row numbers
    LastRowIndex is 0,
    MaxColIndex is NoCols - 1,
    findall(N, between(0, MaxColIndex, N), ColNumbers),
    % Generate a position string for each column number with position_to_string
    findall(Position, (member(ColNumber, ColNumbers), position_to_string(LastRowIndex, ColNumber, NoRows, Position)), Positions).

% get_all_positive_diagonal_starts(+Board, -Starts)
% Predicate to get all the positive diagonal starts of the board
% Starts is a list of position strings
get_all_positive_diagonal_starts(Board, Starts) :-
    get_first_row_positions(Board, RowPositions),
    get_first_column_positions(Board, ColumnPositions),
    append(RowPositions, ColumnPositions, Temp),
    sort(Temp, Starts).

% get_all_negative_diagonal_starts(+Board, -Starts)
% Predicate to get all the negative diagonal starts of the board
% Starts is a list of position strings
get_all_negative_diagonal_starts(Board, Starts) :-
    get_last_row_positions(Board, RowPositions),
    get_first_column_positions(Board, ColumnPositions),
    append(RowPositions, ColumnPositions, Temp),
    sort(Temp, Starts).
    

% get_all_positive_diagonals(+Board, -Diagonals)
% Predicate to get all the positive diagonals of the board
% Diagonals is a list of lists of atoms
get_all_positive_diagonals(Board, Diagonals) :-
    get_all_positive_diagonal_starts(Board, Starts),
    maplist(get_positive_diagonal(Board), Starts, Diagonals).

% get_all_negative_diagonals(+Board, -Diagonals)
% Predicate to get all the negative diagonals of the board
% Diagonals is a list of lists of atoms
get_all_negative_diagonals(Board, Diagonals) :-
    get_all_negative_diagonal_starts(Board, Starts),
    maplist(get_negative_diagonal(Board), Starts, Diagonals).

% get_all_diagonals(+Board, -Diagonals)
% Predicate to get all the diagonals of the board
% Diagonals is a list of lists of atoms
get_all_diagonals(Board, Diagonals) :-
    get_all_positive_diagonals(Board, PositiveDiagonals),
    get_all_negative_diagonals(Board, NegativeDiagonals),
    append(PositiveDiagonals, NegativeDiagonals, Diagonals).

% get_all_board_sequences(+Board, -Sequences)
% Predicate to get all the sequences of the board
% A sequence is a list of stones of the same color
% The list of sequences is a list of lists of stones
get_all_board_sequences(Board, Sequences) :-
    get_all_board_columns(Board, Columns),
    get_all_diagonals(Board, Diagonals),
    append(Board, Columns, RowsAndColumns),
    append(RowsAndColumns, Diagonals, Sequences).


% convert_board_sequences_to_stone_sequences(+Board, +BoardSequences, -StoneSequences)
% Predicate to convert the board sequences to stone sequences
% StoneSequences is a list of lists
convert_board_sequences_to_stone_sequences(_, [], []).
convert_board_sequences_to_stone_sequences(Board, [Head|Tail], StoneSequences) :-
    convert_to_sequences(Head, ConvertedHead),
    convert_board_sequences_to_stone_sequences(Board, Tail, ConvertedTail),
    append(ConvertedHead, ConvertedTail, StoneSequences)
    % .
    , !.

% get_all_stone_sequences(+Board, +Stone, -Sequences)
% Predicate to get all the sequences of the board that contain the stone
% Sequences is a list of lists
get_all_stone_sequences(Board, 'white', Sequences) :-
    get_all_stone_sequences(Board, 'w', Sequences),
    % .
    !.
get_all_stone_sequences(Board, 'black', Sequences) :-
    get_all_stone_sequences(Board, 'b', Sequences),
    % .
    !.

get_all_stone_sequences(Board, Stone, Sequences) :-
    get_all_board_sequences(Board, BoardSequences),
    convert_board_sequences_to_stone_sequences(Board, BoardSequences, StoneSequences),
    include(contains_stone(Stone), StoneSequences, Sequences).

% contains_stone(+Stone, +Sequence)
% Helper predicate to check if a sequence contains a specific stone
contains_stone(Stone, Sequence) :-
    member(Stone, Sequence).

% get_center(+Board, -Center)
% Predicate to get the center of the board
% Center is a position string
get_center(Board, Center) :-
    get_board_size(Board, NoRows, NoCols),
    RowIndex is NoRows // 2,
    ColIndex is NoCols // 2,
    position_to_string(RowIndex, ColIndex, NoRows, Center).

% get_positions_3_away_from_center(+Board, -Positions)
% Predicate to get all the positions that are 3 away from the center
% Positions is a list of position strings
get_positions_3_away_from_center(Board, Positions) :-
    get_all_positions(Board, AllPositions),
    get_center(Board, Center),
    findall(Position,
            (member(Position, AllPositions),
            get_distance(Position, Center, Distance),
            Distance =:= 3),
            Positions).

% is_first_move(+Board)
% Predicate to check if the first move hasn't been made
% Succeeds if the next move is the first move, fails otherwise
is_first_move(Board) :-
    get_no_stones_on_board(Board, TotalStones),
    TotalStones =:= 0.

% is_third_move(+Board)
% Predicate to check if the third move hasn't been made
% Succeeds if the next move is the third move, fails otherwise
is_third_move(Board) :-
    get_no_stones_on_board(Board, TotalStones),
    TotalStones =:= 2.

% get_available_moves(+Board, -Moves)
% Predicate to get the available moves on the board
% Returns the available moves -- a list of positions
get_available_moves(Board, Moves) :-
    get_no_stones_on_board(Board, TotalStones),
    (   TotalStones =:= 0
    ->  get_center(Board, Center),
        Moves = [Center]
    ;   TotalStones =:= 2
    ->  get_positions_3_or_more_away_from_center(Board, Moves)
    ;   get_empty_positions(Board, Moves)
    ).

% get_positions_3_or_more_away_from_center(+Board, -Positions)
% Predicate to get all the positions that are 3 or more away from the center
% Positions is a list of position strings
get_positions_3_or_more_away_from_center(Board, Positions) :-
    get_all_positions(Board, AllPositions),
    get_center(Board, Center),
    findall(Position,
            (member(Position, AllPositions),
            get_distance(Position, Center, Distance),
            Distance >= 3),
            Positions).
        

% get_sequence_score(+Board, +Stone, -Score)
% Predicate to get the score of the stone sequences
% Board is a list of lists without the row/column markers,
% Stone is the stone to be checked, it is either 'O', 'W', or 'B'
% Score is the calculated score -- a number
get_sequence_score(Board, Stone, Score) :-
    get_all_stone_sequences(Board, Stone, AllStoneSequences),

    include(length_equal_to(4), AllStoneSequences, FourSequences),
    include(length_greater_than_or_equal_to(5), AllStoneSequences, FiveOrMoreSequences),

    length(FiveOrMoreSequences, NumFiveOrMore),
    length(FourSequences, NumFour),
    Score is NumFiveOrMore * 5 + NumFour.

% length_greater_than_or_equal_to(+N, +List)
% Predicate to check if the length of List is greater than or equal to N
length_greater_than_or_equal_to(N, List) :-
    length(List, Length),
    Length >= N.

% length_equal_to(+N, +List)
% Predicate to check if the length of List is equal to N
length_equal_to(N, List) :-
    length(List, N).

% valid_position(+Board, +Position)
% Predicate to check if the position is valid
valid_position(Board, Position) :-
    get_board_size(Board, NoRows, NoCols),
    string_to_position(Position, RowIndex, ColIndex),
    RowIndex >= 0,
    RowIndex < NoRows,
    ColIndex >= 0,
    ColIndex < NoCols.



% :- set_prolog_flag(stack, 10000000000).
% :- set_prolog_flag(table_space, 100000000000000000).
% :- table get_empty_board/3.
% :- table get_row/3.
% :- table get_col/3.
% :- table get_char_list/2.
% :- table mark_columns/2.
% :- table mark_rows/2.
% :- table cartesian_board/2.
% % :- table print_board/1.
% :- table get_stone/3.
% :- table get_stones/3.
% :- table set_stone/4.
% :- table convert_to_sequences/2.
% :- table get_empty_positions/2.
% :- table get_no_stones_on_board/2.
% :- table get_all_board_columns/2.
% :- table get_neighboring_stones/3.
% :- table get_up_right_diagonal/3.
% :- table get_up_left_diagonal/3.
% :- table get_down_right_diagonal/3.
% :- table get_down_left_diagonal/3.
% :- table get_positive_diagonal/3.
% :- table get_negative_diagonal/3.
% :- table get_all_positive_diagonal_starts/2.
% :- table get_all_negative_diagonal_starts/2.
% :- table get_first_row_positions/2.
% :- table get_first_column_positions/2.
% :- table get_last_row_positions/2.
% :- table get_all_positive_diagonals/2.
% :- table get_all_negative_diagonals/2.
% :- table get_all_diagonals/2.
% :- table get_all_board_sequences/2.
% :- table convert_board_sequences_to_stone_sequences/3.
% :- table get_all_stone_sequences/3.
% :- table contains_stone/2.
% :- table get_board_size/3.
% :- table get_center/2.
% :- table get_positions_3_away_from_center/2.
% :- table is_first_move/1.
% :- table is_third_move/1.
% :- table get_available_moves/2.
% :- table get_all_positions/2.
% :- table get_sequence_score/3.
% :- table valid_position/2.
% :- table length_greater_than_or_equal_to/2.