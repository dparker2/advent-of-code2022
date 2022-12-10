% row([2,3,4]).
:- dynamic row/2.

assert_rows([], _).
assert_rows([RowStr|Rest], Index) :-
  string_chars(RowStr, RowChars),
  maplist(atom_number, RowChars, Row),
  assertz(row(Index, Row)),
  NextIndex is Index + 1,
  assert_rows(Rest, NextIndex).


column(Index, Column) :-
  once(row(0, R0)),
  nth0(Index, R0, _),
  findall(X, (row(_, R), nth0(Index, R, X)), Column).


unblocked_before(0, _).
unblocked_before(Index, Treeline) :-
  nth0(Index, Treeline, Height),
  length(Before, Index),
  prefix(Before, Treeline),
  max_list(Before, Tallest),
  Tallest < Height.


unblocked_after(Index, Treeline) :- length(Treeline, Len), Index is Len - 1.
unblocked_after(Index, Treeline) :-
  nth0(Index, Treeline, Height),
  length(Treeline, N),
  AfterLength is N - Index - 1,
  length(After, AfterLength),
  append(_, After, Treeline),
  max_list(After, Tallest),
  Tallest < Height.


visible_from(X, Y, top) :- column(Y, Col), unblocked_before(X, Col).
visible_from(X, Y, bottom) :- column(Y, Col), unblocked_after(X, Col).
visible_from(X, Y, left) :- row(X, Row), unblocked_before(Y, Row).
visible_from(X, Y, right) :- row(X, Row), unblocked_after(Y, Row).


part1(InputFile, VisibleCount) :-
  open(InputFile, read, Stream),
  read_string(Stream, _, String),
  split_string(String, "\n", "\n", Treelines),
  retractall(row(_,_)),
  assert_rows(Treelines, 0),
  findall([X, Y], visible_from(X, Y, _), VisibleCoords),
  list_to_set(VisibleCoords, VisibleSet),
  length(VisibleSet, VisibleCount).
