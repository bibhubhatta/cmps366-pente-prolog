% Predicate to convert position to string representation
% Assistance: https://www.swi-prolog.org/pldoc/man?predicate=char_code/2
% https://www.swi-prolog.org/pldoc/doc_for?object=atom_concat/3
position_to_string(RowIndex, ColIndex, NumRows, PositionString) :-
    % Convert column number to corresponding ASCII character (A=65, B=66, ...)
    ColDisplayCharVal is ColIndex + 1 + 64,
    char_code(ColDisplayChar, ColDisplayCharVal),

    RowDisplayNum is NumRows - RowIndex,
    atom_concat(ColDisplayChar, RowDisplayNum, PositionString). 


% Predicate to convert position to string representation for default board size (19)
% 
position_to_string(RowIndex, ColIndex, PositionString) :-
    position_to_string(RowIndex, ColIndex, 19, PositionString).
    

% Predicate to convert string representation of position to row and column indices
% Assistance: https://www.swi-prolog.org/pldoc/doc_for?object=atom_length/2
% https://www.swi-prolog.org/pldoc/doc_for?object=sub_atom/5
% https://www.swi-prolog.org/pldoc/doc_for?object=atom_number/2
string_to_position(PositionString, NumRows, RowIndex, ColIndex) :-
    % Convert column character to corresponding ASCII value (A=65, B=66, ...)
    sub_atom(PositionString, 0, 1, _, ColDisplayChar),
    char_code(ColDisplayChar, ColDisplayCharVal),
    ColIndex is ColDisplayCharVal - 64 - 1,

    % Convert row number to corresponding ASCII value (1=49, 2=50, ...)
    atom_length(PositionString, Length),
    EndIndex is Length - 1,
    sub_atom(PositionString, 1, EndIndex, _, RowDisplayNum),
    atom_number(RowDisplayNum, RowDisplayNumVal),
    RowIndex is NumRows - RowDisplayNumVal.


% Predicate to convert string representation of position to row and column indices for default board size (19)
string_to_position(PositionString, RowIndex, ColIndex) :-
    string_to_position(PositionString, 19, RowIndex, ColIndex).




% Predicate to get the position above the given position
up_position(PositionString, UpPositionString) :-
    string_to_position(PositionString, RowIndex, ColIndex),
    UpRowIndex is RowIndex - 1,
    position_to_string(UpRowIndex, ColIndex, UpPositionString).

% Predicate to get the position below the given position
down_position(PositionString, DownPositionString) :-
    string_to_position(PositionString, RowIndex, ColIndex),
    DownRowIndex is RowIndex + 1,
    position_to_string(DownRowIndex, ColIndex, DownPositionString).

% Predicate to get the position to the left of the given position
left_position(PositionString, LeftPositionString) :-
    string_to_position(PositionString, RowIndex, ColIndex),
    LeftColIndex is ColIndex - 1,
    position_to_string(RowIndex, LeftColIndex, LeftPositionString).

% Predicate to get the position to the right of the given position
right_position(PositionString, RightPositionString) :-
    string_to_position(PositionString, RowIndex, ColIndex),
    RightColIndex is ColIndex + 1,
    position_to_string(RowIndex, RightColIndex, RightPositionString).




