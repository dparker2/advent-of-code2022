/*
Find path via A*
*/
:- dynamic node/2.  % node([1,1], a).

can_move(83, 97).  % Can move from 'S' to a or b
can_move(83, 98).
can_move(121, 69). % Can move to 'E' from y or z
can_move(122, 69).
can_move(V, V2) :- maplist(between(97, 122), [V,V2]), V2 - V =< 1.

edge([X,Y], [X2,Y2]) :- node([X,Y], V), node([X2,Y2], V2),
  ( Y = Y2, Xd is abs(X-X2), Xd = 1;
    X = X2, Yd is abs(Y-Y2), Yd = 1 ),
  can_move(V, V2).

load_facts(Rows) :-
  nth1(I, Rows, Row), string_chars(Row, Chars),
  nth1(J, Chars, Char), char_code(Char, V), assertz(node([I,J], V)).

init(InputFile) :- retractall(node(_,_)),
  open(InputFile, read, Stream),
  read_string(Stream, _, String),
  atomic_list_concat(Rows, '\n', String),
  findall(_, load_facts(Rows), _).

euclidean([X,Y], [X2,Y2], Out) :- Out is sqrt((X-X2)^2 + (Y-Y2)^2).

expand_path(EndXY, XY, Heap, List) :- length([XY|Path], L),
  findall(
    P-Next,
    ( edge(XY, Next), \+ get_from_heap(Heap, _, Next, _),
      euclidean(Next, EndXY, E), P is E+L ),
    List
  ).

build_path(XY, PathDict, Path) :- ( Prev = PathDict.get(XY)
  -> build_path(Prev, PathDict, Rest), Path is [XY|Rest]
  ;  Path is [] ).

% FIXME: Heap = open set (node to explore), cameFrom = current min preceding node
search(Heap0, EndXY, Path) :- get_from_heap(Heap0, _, MinNode, Heap),
  % writeln(MinPath),
  ( MinNode = EndXY
    -> Path = MinNode
    ; (
      expand_path(EndXY, MinNode, PathList),
      list_to_heap(PathList, PathHeap),
      merge_heaps(Heap, PathHeap, NextHeap),
      search(NextHeap, EndXY, Path)
    )
  ).

% If multiple Start or End with same value, will solve all shortest paths
shortest_path(Start, End, Path) :- node(StartXY, Start), node(EndXY, End),
  expand_path(EndXY, [StartXY], List0),
  list_to_heap(List0, Heap0),
  search(Heap0, EndXY, Path).

part1(InputFile, Steps) :- init(InputFile), shortest_path(0'S, 0'E, Path),
  length(Path, Length), Steps is Length - 1.
