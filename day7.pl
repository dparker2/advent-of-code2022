:- dynamic directory/1.
:- dynamic file/3.
/*
Filesystem example:
directory([root]).
directory([root, foo]).
file(bar, 1024, [root, foo]).  % bar is a file of size 1024 in root/foo
*/

% ls and doesn't change anything
assert_output(['$',' ','l','s'], CurrPath, NextPath) :-
  NextPath = CurrPath.

% Path changes
assert_output(['$',' ','c','d',' ','.','.'], CurrPath, NextPath) :-
  append(NextPath, [_], CurrPath).  % Pop CurrPath to get Next

assert_output(['$',' ','c','d',' '|DirChars], CurrPath, NextPath) :-
  DirChars \= ['.','.'],
  string_chars(Dir, DirChars),  % Next is what's after cd
  append(CurrPath, [Dir], NextPath).

% Assert file exists
assert_output(FilelineChars, CurrPath, NextPath) :-
  NextPath = CurrPath,  % Not moving
  string_chars(Fileline, FilelineChars),
  split_string(Fileline, ' ', ' ', [SizeStr, Filename]),
  atom_number(SizeStr, Filesize),
  assertz(file(Filename, Filesize, CurrPath)).

% Assert dir exists
assert_output(['d','i','r',' '|DirChars], CurrPath, NextPath) :-
  NextPath = CurrPath,
  string_chars(Dir, DirChars),
  append(CurrPath, [Dir], NewPath),
  assertz(directory(NewPath)).


build_filesystem(_, []).
build_filesystem(CurrPath, [Output|Rest]) :-
  string_chars(Output, OutputChars),
  assert_output(OutputChars, CurrPath, NextPath),
  build_filesystem(NextPath, Rest).


file_within(File, Path, Filesize) :-
  file(File, Filesize, Filepath),
  append(Path, _, Filepath).  % Starts with path?


directory_size(Path, Size) :-
  directory(Path),
  findall(X, file_within(_, Path, X), Filesizes),
  sum_list(Filesizes, Size).


initialize(InputFile) :-
  open(InputFile, read, Stream),
  read_string(Stream, _, String),
  split_string(String, "\n", "\n", Lines),
  retractall(file(_, _, _)),
  retractall(directory(_)),
  assertz(directory(["/"])),  % Add the root
  build_filesystem([], Lines),!.


part1(InputFile, TotalSize) :-
  initialize(InputFile),
  findall(X, (directory_size(_, X), X =< 100000), Sizes),
  sum_list(Sizes, TotalSize).


part2(InputFile, DeleteDirSize) :-
  initialize(InputFile),
  directory_size(["/"], UsedSpace),
  UnusedSpace = 70000000 - UsedSpace,
  NeededSpace = 30000000 - UnusedSpace,
  findall(X, (directory_size(_, X), X >= NeededSpace), Sizes),
  min_list(Sizes, DeleteDirSize).
