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
    get_empty_positions/2
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
    % Set the stone
    nth0(RowIndex, Board, Row),
    nth0(ColIndex, NewRow, Stone, Row),
    nth0(RowIndex, NewBoard, NewRow).


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
    findall(Position,
            (between(0, NoRows, RowIndex), 
            between(0, NoCols, ColIndex),
            position_to_string(RowIndex, ColIndex, Position)), 
            Positions).

% get_empty_positions(+Board, -Positions)
% Purpose: Get the empty positions on the board
get_empty_positions(Board, Positions) :-
    get_all_positions(Board, AllPositions),
    findall(Position,
            (member(Position, AllPositions),
            get_stone(Board, Position, 'o')),
            Positions).