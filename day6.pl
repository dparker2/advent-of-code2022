is_unique_run(Signal, Position, Length) :-
  sub_string(Signal, Start, Length, _, Substr),
  Position is Start + Length,
  string_codes(Substr, Codes),
  is_set(Codes).

find_unique_run(File, Position, Length) :-
  open(File, read, Stream),
  read_string(Stream, _, Signal),
  once(is_unique_run(Signal, Position, Length)).

part1(File, Pos) :- find_unique_run(File, Pos, 4).
part2(File, Pos) :- find_unique_run(File, Pos, 14).
