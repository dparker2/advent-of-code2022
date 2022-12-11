% tree([2, 2], 5).  Tree at (2,2) of height 5
:- dynamic tree/2.
:- dynamic grid_size/1.

assert_tree([], _, _).
assert_tree([Height|Rest], X, Y) :-
  assertz(tree([X, Y], Height)),
  X2 is X + 1, 
  retractall(grid_size(_)), assertz(grid_size(X2)),
  assert_tree(Rest, X2, Y).

assert_trees([], _).
assert_trees([RowStr|Rest], Y) :-
  string_chars(RowStr, RowChars),
  maplist(atom_number, RowChars, Row),
  assert_tree(Row, 0, Y),
  Y2 is Y + 1, assert_trees(Rest, Y2).

move_up([X, Y], [X2, Y2]) :- X2 is X - 1, Y2 is Y.
move_down([X, Y], [X2, Y2]) :- X2 is X + 1, Y2 is Y.
move_left([X, Y], [X2, Y2]) :- X2 is X, Y2 is Y - 1.
move_right([X, Y], [X2, Y2]) :- X2 is X, Y2 is Y + 1.

% Surrounded by trees!
inner(XY) :- tree(XY, _),
  move_up(XY, XYUp), tree(XYUp, _),
  move_right(XY, XYRight), tree(XYRight, _),
  move_down(XY, XYDown), tree(XYDown, _),
  move_left(XY, XYLeft), tree(XYLeft, _).

% Tree but not surrounded
edge(XY) :- tree(XY, _), \+ inner(XY).

count_trees(XY, MovePred, Goal, Count) :-
  (
    call(MovePred, XY, XY2),
    tree(XY2, H),!,
    call(Goal, H),
    count_trees(XY2, MovePred, Goal, NextCount),
    Count is NextCount + 1
  );
  Count is 0.

unblocked_trees(Count) :-
  inner([X,Y]),
  tree([X,Y], Height),
  once(grid_size(N)),
  ClearView is [H] >> (H < Height),
  (
    count_trees([X,Y], move_up, ClearView, Count), Count = Y;
    count_trees([X,Y], move_right, ClearView, Count), Count = ;
    count_trees([X,Y], move_down, ClearView, Count);
    count_trees([X,Y], move_left, ClearView, Count);
  ).

part1(InputFile, VisibleCount) :-
  open(InputFile, read, Stream),
  read_string(Stream, _, String),
  split_string(String, "\n", "\n", Treelines),
  retractall(row(_,_)),
  assert_trees(Treelines, 0).
  findall(_, edge(_), Edges),
  findall(X, scan_trees())
  % findall([X, Y], visible_from(X, Y, _), VisibleCoords),
  % list_to_set(VisibleCoords, VisibleSet),
  % length(VisibleSet, VisibleCount).
