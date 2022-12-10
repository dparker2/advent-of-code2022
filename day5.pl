/* Load state into facts */
:- dynamic stack/2.  % stack('M', 3). Position = order of assertion

parse_command(CommandStr, Command) :-
  atomic_list_concat([_, NStr, _, FromStr, _, ToStr], ' ', CommandStr),
  maplist(atom_number, [NStr, FromStr, ToStr], Command).

% [Z] [M] -> stack(Z, 1). stack(M, 2). etc
load_stackline([]).
load_stackline([Stackline|Rest]) :-
  (
    string_code(Index, Stackline, Code),
    code_type(Code, alpha), % Only get the letters
    StackIndex is (Index-2) / 4 + 1,
    char_code(Char, Code),
    assertz(stack(Char, StackIndex))
  );
  load_stackline(Rest).

load_file(File, Commands, StackNums) :-
  retractall(stack(_,_)),
  open(File, read, Stream),
  read_string(Stream, _, String),
  atomic_list_concat([StacksStr, CommandsStr], '\n\n', String),
  atomic_list_concat(StacksList, '\n', StacksStr),
  append(Stacklines, [StackNumsStr], StacksList), % Remove stack numbers
  split_string(StackNumsStr, ' ', ' ', StackNumsSplit),
  maplist(atom_number, StackNumsSplit, StackNums),
  findall(_, load_stackline(Stacklines), _),!,
  atomic_list_concat(CommandsList, '\n', CommandsStr),
  maplist(parse_command, CommandsList, Commands).

move_crate(From, To, Crate) :-
  once(retract(stack(Crate, From))),
  asserta(stack(Crate, To)).

move_crates(CratesFn, [N, From, To]) :-
  top_n_crates(N, From, Cs),
  call(CratesFn, Cs, Crates),
  maplist(move_crate(From, To), Crates).

top_n_crates(N, StackNum, Crates) :- findnsols(N, X, stack(X, StackNum), Crates).

run(File, TopCrates, CratesFn) :-
  load_file(File, Commands, StackNums),
  maplist(move_crates(CratesFn), Commands),!,
  maplist(top_n_crates(1), StackNums, Tops),!,
  flatten(Tops, FlatTops),
  atomic_list_concat(FlatTops, '', TopCrates).

part1(File, TopCrates) :- run(File, TopCrates, [A,B]>>(A=B)).
part2(File, TopCrates) :- run(File, TopCrates, reverse).
