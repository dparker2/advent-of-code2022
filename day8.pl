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

move_up([X, Y], [X2, Y2]) :- X2 is X, Y2 is Y - 1.
move_right([X, Y], [X2, Y2]) :- X2 is X + 1, Y2 is Y.
move_down([X, Y], [X2, Y2]) :- X2 is X, Y2 is Y + 1.
move_left([X, Y], [X2, Y2]) :- X2 is X - 1, Y2 is Y.

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
    tree(XY2, H),
    call(Goal, H),
    count_trees(XY2, MovePred, Goal, NextCount),
    Count is NextCount + 1
  );
  Count is 0.

% So inefficient.
unblocked_trees(XY) :- edge(XY).
unblocked_trees([X,Y]) :-
  inner([X,Y]),
  tree([X,Y], Height),
  once(grid_size(N)),
  ClearView = [H] >> (H < Height),
  (
    count_trees([X,Y], move_up, ClearView, Count), Count is Y;
    count_trees([X,Y], move_right, ClearView, Count), Count is N - X - 1;
    count_trees([X,Y], move_down, ClearView, Count), Count is N - Y - 1;
    count_trees([X,Y], move_left, ClearView, Count), Count is X
  ).

part1(InputFile, VisibleCount) :-
  open(InputFile, read, Stream),
  read_string(Stream, _, String),
  split_string(String, "\n", "\n", Treelines),
  retractall(tree(_,_)),
  assert_trees(Treelines, 0),
  findall(XY, unblocked_trees(XY), List),
  list_to_set(List, Set),
  length(Set, VisibleCount).


scenic_score(XY, Score) :-
  tree(XY, Height),
  InView = [H] >> (H >= Height),
  count_trees(XY, move_up, InView, Count1),
  writeln(Count1),
  count_trees(XY, move_right, InView, Count2),
  count_trees(XY, move_down, InView, Count3),
  count_trees(XY, move_left, InView, Count4),
  Score is Count1 * Count2 * Count3 * Count4.
