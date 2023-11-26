:- module(position, [
    position_to_string/3,
    position_to_string/4,
    string_to_position/4,
    string_to_position/3,
    up_position/2,
    down_position/2,
    left_position/2,
    right_position/2,
    up_left_position/2,
    up_right_position/2,
    down_left_position/2,
    down_right_position/2,
    get_neighbors/2,
    valid_position/1,
    get_distance/3
    ]).

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
    % Convert PositionString to uppercase
    upcase_atom(PositionString, UpperCasePositionString),
    % Convert column character to corresponding ASCII value (A=65, B=66, ...)
    sub_atom(UpperCasePositionString, 0, 1, _, ColDisplayChar),
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
    % UpRowIndex >= 0,
    position_to_string(UpRowIndex, ColIndex, UpPositionString).

% Predicate to get the position below the given position
down_position(PositionString, DownPositionString) :-
    string_to_position(PositionString, RowIndex, ColIndex),
    DownRowIndex is RowIndex + 1,
    % DownRowIndex < 19,
    position_to_string(DownRowIndex, ColIndex, DownPositionString).

% Predicate to get the position to the left of the given position
left_position(PositionString, LeftPositionString) :-
    string_to_position(PositionString, RowIndex, ColIndex),
    LeftColIndex is ColIndex - 1,
    % LeftColIndex >= 0,
    position_to_string(RowIndex, LeftColIndex, LeftPositionString).

% Predicate to get the position to the right of the given position
right_position(PositionString, RightPositionString) :-
    string_to_position(PositionString, RowIndex, ColIndex),
    RightColIndex is ColIndex + 1,
    % RightColIndex < 19,
    position_to_string(RowIndex, RightColIndex, RightPositionString).


% Predicate to get the position above and to the left of the given position
up_left_position(PositionString, UpLeftPositionString) :-
    up_position(PositionString, UpPositionString),
    left_position(UpPositionString, UpLeftPositionString).

% Predicate to get the position above and to the right of the given position
up_right_position(PositionString, UpRightPositionString) :-
    up_position(PositionString, UpPositionString),
    right_position(UpPositionString, UpRightPositionString).

% Predicate to get the position below and to the left of the given position
down_left_position(PositionString, DownLeftPositionString) :-
    down_position(PositionString, DownPositionString),
    left_position(DownPositionString, DownLeftPositionString).

% Predicate to get the position below and to the right of the given position
down_right_position(PositionString, DownRightPositionString) :-
    down_position(PositionString, DownPositionString),
    right_position(DownPositionString, DownRightPositionString).

% get_neighbors(+PositionString, -Neighbors)
% Predicate to get all the neighbors of the given position
% Neighbors is a list of all the neighbors of the given position in clockwise order starting from the top
get_neighbors(PositionString, Neighbors) :-
    findall(Neighbor, (
        (
            up_position(PositionString, Neighbor); 
            up_right_position(PositionString, Neighbor); 
            right_position(PositionString, Neighbor); 
            down_right_position(PositionString, Neighbor); 
            down_position(PositionString, Neighbor);
            down_left_position(PositionString, Neighbor); 
            left_position(PositionString, Neighbor);
            up_left_position(PositionString, Neighbor)
        ),

        valid_position(Neighbor)

    ), Neighbors).


% valid_position(+PositionString)
% Predicate to check if the given position is valid
valid_position(PositionString) :-
    string_to_position(PositionString, RowIndex, ColIndex),
    RowIndex >= 0,
    RowIndex < 19,
    ColIndex >= 0,
    ColIndex < 19.

% get_distance(+Position1, +Position2, -Distance)
% Predicate to get the distance between two positions
% Distance is the number of positions between the two positions
get_distance(Position1, Position2, Distance) :-
    string_to_position(Position1, Row1, Col1),
    string_to_position(Position2, Row2, Col2),
    Distance is max(abs(Row1 - Row2), abs(Col1 - Col2)).