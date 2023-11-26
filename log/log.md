# Project Log

## Nov 21, 2023

- Learn about prolog 0.75
- Setup swipl and watch tutorials on YouTube 1.0
- Find and install VS Code extensions for Prolog, so that it can be developed across operating systems. 1
- Using VSC-Prolog by arthurwang for syntax highlighting
- Install swipl for running prolog from the terminal: use “swipl \<filename>” to run the script from the terminal

Total: 2.75  

## Nov 22, 2023

- Test and implement position related predicates
  - Position_to_string, string_to_position, up_position, down_position, left_position, right_position 2.0
- Copy and paste serialization files 0.5
  - Prof's serialization files used ; instead of % for comments, so replaced it
- Test and implement predicates to load and save the game state 0.5
- Test and implement predicates to get the constituent parts of the game state 0.5
- Test and implement predicates to get the current player and stone 0.5
- Test and implement predicates to get the player's tournament score, captured pairs, as well as to check if the stone is valid, to set the captures and tournament scores 0.75
- Test and implement predicates to set the current player and stone and switch the turn 0.5

Total: 5.25

## Nov 23, 2023

- Test and implement predicate to get initial game state 1.0
- Test and implement predicate to get the capture sequence for a stone 0.25

Total: 1.25

## Nov 24, 2023

- Test and implement predicate to get row 0.25
- Test and implement predicate to get column 0.5
- Test and implement predicate to mark column characters at the end 1.0
- Test and implement predicate to mark row numbers 0.75
- Refactor mark_rows so there are no choice points 0.5
- Test and implement predicate to mark the board 0.5

Total: 3.5

## Nov 25, 2023

- Test and implement predicate to print the board 1
- Refactor to use modules 2
  - Using modules because consult leads to problems when the same file is consulted multiple times
- Test and implement predicate to get stone at position 0.75
- Test and implement predicate to get a list of stones from the position list 0.25
- Test and implement predicate to set stone at position 0.5
- Test and implement predicate to convert a board sequence to a sequence of stones, i.e. list of lists 0.5
- Test and implement predicate to get_empty_positions 1
- Test and update set_stone when using 'white', 'black', and 'empty' instead of 'w', 'b', and 'o' 0.5
  - This uses cut so that the Prolog interpreter doesn't think that the predicate has multiple solutions
- Test and implement get_no_stones_on_board 0.5
  - Uses is_stone as a helper predicate
- Test and implement predicate to get_all_board_columns 0.5
- Test and implement predicate to get diagonal positions 0.5
  - Implement bound checking for up, down, left, and right positions
- Test and implement predicates to get_neighbors 0.75
  - Bound validation for up, down, left, and right positions are removed because valid_position now checks for bounds; this makes it easy to get the neighbors of a position
- Test and implement predicate to get_neighboring_stones 0.5
- Test and implement predicate to get diagonals 1.5
