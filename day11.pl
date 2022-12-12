/*
Monkey 0 has these items:
item(0, 65).
item(0, 58).
And this worry operation:
operation(0, "old * 7").
And throws to 6 if divisible by 19, else 4:
throw_test(0, 19, 6, 4).
*/
:- dynamic monkey/1.
:- dynamic item/2.
:- dynamic operation/2.
:- dynamic throw_test/4.
:- dynamic inspected/1.
:- dynamic part/1.
:- dynamic common_mod/1.

% monkey(0).
% monkey(3).
% item(0, 79).
% item(0, 98).
% operation(0, "old * 19").
% operation(3, "old + 3").
% throw_test(0, 23, 2, 3).
% throw_test(3, 17, 0, 1).

inspect(Monkey, OldWorry, NewWorry) :-
  operation(Monkey, Op),
  re_replace("old"/g, OldWorry, Op, S),
  term_to_atom(T, S),
  ( part(1)
    -> NewWorry is div(T, 3)
    ;  common_mod(X), NewWorry is T mod X
  ),
  assertz(inspected(Monkey)).

throw(Monkey, ItemWorry, ThrowTo, NewWorry) :-
  % format('~d inspecting ~d~n', [Monkey, ItemWorry]),
  inspect(Monkey, ItemWorry, NewWorry),
  throw_test(Monkey, Div, MTrue, MFalse),
  ( NewWorry mod Div =:= 0
    -> ThrowTo is MTrue
    ;  ThrowTo is MFalse
  ).

do_round() :-
  monkey(Monkey),
  item(Monkey, ItemWorry),
  throw(Monkey, ItemWorry, ThrowTo, NewWorry),
  retract(item(Monkey, ItemWorry)),
  % format('~d threw item ~d to ~d~n', [Monkey, NewWorry, ThrowTo]),
  assertz(item(ThrowTo, NewWorry)).

load_facts(MonkeySpec) :-
  re_matchsub('Monkey (?<m_I>\\d+):
  Starting items: (?<items_S>.+)
  Operation: new = (?<op_S>.+)
  Test: divisible by (?<div_I>\\d+)
    If true: throw to monkey (?<mTrue_I>\\d+)
    If false: throw to monkey (?<mFalse_I>\\d+)', MonkeySpec, Sub, []),
  atomic_list_concat(Items, ', ', Sub.items),
  assertz(monkey(Sub.m)),
  findall(_, (member(S, Items), atom_number(S, I), assertz(item(Sub.m, I))), _),
  assertz(operation(Sub.m, Sub.op)),
  assertz(throw_test(Sub.m, Sub.div, Sub.mTrue, Sub.mFalse)).

init(InputFile) :-
  retractall(monkey(_)), retractall(item(_,_)), retractall(operation(_,_)),
  retractall(throw_test(_,_,_,_)), retractall(inspected(_)), retractall(part(_)),
  open(InputFile, read, Stream),
  read_string(Stream, _, String),
  atomic_list_concat(L, '\n\n', String),
  maplist(load_facts, L).

inspect_count(Monkey, Count) :-
  monkey(Monkey), findall(_, inspected(Monkey), L), length(L, Count).

monkey_business(MonkeyBusiness) :-
  findall(X, inspect_count(_, X), InspectionList),
  sort(0, @>=, InspectionList, [Top1,Top2|_]),
  MonkeyBusiness is Top1 * Top2.

part1(InputFile, MonkeyBusiness) :-
  init(InputFile), assertz(part(1)),
  findall(_, (between(1, 20, _), do_round), _),
  monkey_business(MonkeyBusiness).

mult_list([], X) :- X is 1.
mult_list([H|T], X) :- mult_list(T, X2), X is X2 * H.

part2(InputFile, MonkeyBusiness) :-
  init(InputFile), assertz(part(2)), retractall(common_mod(_)),
  % Multiply all modulos together to find common Mod
  findall(X, throw_test(_,X,_,_), Mods), mult_list(Mods, Mod), assertz(common_mod(Mod)),
  findall(_, (between(1, 10000, _), do_round), _),
  monkey_business(MonkeyBusiness).
