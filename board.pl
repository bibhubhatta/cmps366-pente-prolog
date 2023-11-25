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