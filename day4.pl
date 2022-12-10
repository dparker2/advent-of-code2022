extract_ranges(AssignmentRow, Min1, Max1, Min2, Max2) :-
  row(A1, A2) = AssignmentRow,
  atomic_list_concat(MinMax1, '-', A1),
  atomic_list_concat(MinMax2, '-', A2),
  maplist(atom_number, MinMax1, [Min1, Max1]),
  maplist(atom_number, MinMax2, [Min2, Max2]).

fully_contains(AssignmentRow) :-
  extract_ranges(AssignmentRow, Min1, Max1, Min2, Max2),
  (
    % Either range min AND max between other range
    between(Min1, Max1, Min2), between(Min1, Max1, Max2);
    between(Min2, Max2, Min1), between(Min2, Max2, Max1)
  ).

partially_contains(AssignmentRow) :-
  extract_ranges(AssignmentRow, Min1, Max1, Min2, Max2),
  (
    % Either range min OR max between other range
    between(Min1, Max1, Min2); between(Min1, Max1, Max2);
    between(Min2, Max2, Min1); between(Min2, Max2, Max1)
  ).

num_rows(File, FilterPredicate, N) :-
  csv_read_file(File, Rows),
  include(FilterPredicate, Rows, ContainingRows),
  length(ContainingRows, N).

part1(File, X) :- num_rows(File, fully_contains, X).
part2(File, X) :- num_rows(File, partially_contains, X).
