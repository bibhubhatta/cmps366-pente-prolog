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
