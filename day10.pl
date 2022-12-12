:- dynamic cycle/2.

strength(N, Strength) :- cycle(N, Sig), Strength is N * Sig.

simulate_cycle([], _, _).
% Noop
simulate_cycle([[n,o,o,p]|Commands], Cycle, Sig) :-
  assertz(cycle(Cycle, Sig)),
  Cycle2 is Cycle + 1,
  simulate_cycle(Commands, Cycle2, Sig).
% Add command
simulate_cycle([[a,d,d,x,' '|ArgChs]|Commands], Cycle, Sig) :-
  assertz(cycle(Cycle, Sig)),  % noop cycle
  string_chars(ArgStr, ArgChs), atom_number(ArgStr, ArgNum),
  Sig2 is Sig + ArgNum, Cycle2 is Cycle + 1, Cycle3 is Cycle2 + 1,
  assertz(cycle(Cycle2, Sig2)),  % finished cycle
  simulate_cycle(Commands, Cycle3, Sig2).

init(InputFile) :-
  retractall(cycle(_,_)),
  assertz(cycle(1,1)),  % X=1 for Cycle 1
  open(InputFile, read, Stream),
  read_string(Stream, _, String),
  split_string(String, "\n", "\n", Lines),
  maplist(string_chars, Lines, Commands),
  simulate_cycle(Commands, 2, 1).

part1(InputFile, Sum) :-
  init(InputFile),
  findall(X, (member(C, [20,60,100,140,180,220]), strength(C, X)), Strs),
  sum_list(Strs, Sum).

pixel(Cycle, Pixel) :-
  PixelPosition is (Cycle - 1) mod 40,
  cycle(Cycle, Signal),
  PLow is Signal - 1, PHigh is Signal + 1,
  ( between(PLow, PHigh, PixelPosition)
    -> Pixel is "#"
    ;  Pixel is "."
  ).

crt_row(Row) :-
  CLow is 1 + (40 * Row), CHigh is CLow + 39,
  findall(X, (between(CLow, CHigh, C), pixel(C, X)), Pixels),
  string_chars(Line, Pixels), writeln(Line).

part2(InputFile) :-
  init(InputFile),
  findall(X, between(0, 5, X), Rows),
  maplist(crt_row, Rows).
