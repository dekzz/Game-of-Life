% Conway's Game of Life

:- module(life, [start/0]).

start :-
    grid(X),
    life(X).
	
% The life grid, 20x20
grid([  
		[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
		[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
     ]
   ).

% Infinite generates with keystroke
% -------------------------------------
life(Grid) :-
    dumpgen(Grid),				% Write the grid to console
    onegen(Grid, 0, NewGrid),	% New generation
    get_single_char(_),			% Wait for user interaction
    life(NewGrid).				% Next step

	
% Dumps a generation out
% ----------------------
dumpgen([]) :- nl.		% Write empty line for empty grid
dumpgen([H|T]) :-		% Case in which grid is not empty
    write(H), nl,		% Write the first element and new line
    dumpgen(T).			% Next step

	
% Does one generation
% --------------------------------
onegen(_, 20, []).

onegen(Grid, Row, [NewRow|NewGrid]) :-	% Params (grid, row, new grid)
    xformrow(Grid, Row, 0, NewRow),		% Transform the row values
    NRow is Row + 1,					% Step row
    onegen(Grid, NRow, NewGrid).		% Recursion for all rows

	
% Transforms one row
% --------------------------------
xformrow(_, _, 20, []).
xformrow(Grid, Row, Col, [NewState|NewList]) :-	% Params(grid, row, column, nRow)
    xformstate(Grid, Row, Col, NewState),		% Edite cell state
    NewCol is Col + 1,							% Step column
    xformrow(Grid, Row, NewCol, NewList).		% Recursion for all columns


% Request new state of any cell
% --------------------------------
xformstate(Grid, Row, Col, NS) :-		% Params(grid, row, column, new state)
    cellstate(Grid, Row, Col, CS),		% Get current cell state
    nextstate(Grid, Row, Col, CS, NS).	% Calculate the next state

	
% Calculate next state of any cell
% --------------------------------

% Cell is currently dead
nextstate(Grid, Row, Col, 0, NS) :-		% Params(grid, row, column, currState, nState)
    neightotal(Grid, Row, Col, Total),	% Check the neighbours
    (Total =:= 3 -> NS = 1 ; NS = 0).	% If 3 alive neighbours, new state is 1 
										% (alive), else 0 (dead) <currState = 0>

% Cell is currently alive
nextstate(Grid, Row, Col, 1, NS) :-		% Params(grid, row, column, currState, nState)
    neightotal(Grid, Row, Col, Total),	% Check the neighbours
    ((Total =:= 2; Total =:=3)			% If 2 or 3 alive neighbours, new state is 1 
    -> NS = 1; NS = 0).					% (alive), else cell dies

	
% State of all surrounding neighbours
%-------------------------------------
neightotal(Grid, Row, Col, TotalSum) :- % Params(grid, row, column, totalAlive)

    % Immediately neighbours X, Y
    XM1 is Col - 1,						% Left neighbour
    XP1 is Col + 1,						% Right neighbour
    YM1 is Row - 1,						% Top neighbour
    YP1 is Row + 1,						% Bottom neighbour

    % State at all those compass points
    cellstate(Grid, YM1, Col, N),
    cellstate(Grid, YM1, XP1, NE),
    cellstate(Grid, Row, XP1, E),
    cellstate(Grid, YP1, XP1, SE),
    cellstate(Grid, YP1, Col, S),
    cellstate(Grid, YP1, XM1, SW),
    cellstate(Grid, Row, XM1, W),
    cellstate(Grid, YM1, XM1, NW),

    % Add up the liveness
    TotalSum is N + NE + E + SE + S + SW + W + NW.

	
% State at any given row / col - 0 or 1
% -----------------------------------
% Valid range, return it's state
cellstate(Grid, Row, Col, State) :-	% Params(grid, row, column, state)
    between(0, 19, Row),			% Go through rows
    between(0, 19, Col),			% Go through columns
    nth0(Row, Grid, RL),			% Check every cell and
    nth0(Col, RL, State).			% return it's state

	
% Outside range is dead
cellstate(_, _, _, 0).	% Outside of the „visible grid“
