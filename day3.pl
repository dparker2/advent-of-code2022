common_item_part1([Row|Rest], Item) :-
  row(Bag) = Row,
  (
    string_chars(Bag, List),
    append(First, Second, List),
    length(First, L),
    length(Second, L),
    intersection(First, Second, Common),
    Common = [Item|_]  % Always 1 item
  );
  common_item_part1(Rest, Item).

common_item_part2([Row1,Row2,Row3|Rest], Item) :-
  row(Bag1) = Row1, string_chars(Bag1, List1),
  row(Bag2) = Row2, string_chars(Bag2, List2),
  row(Bag3) = Row3, string_chars(Bag3, List3),
  (
    intersection(List1, List2, Common12),
    intersection(Common12, List3, Common),
    Common = [Item|_]
  );
  common_item_part2(Rest, Item).

priority(Item, Value) :- 
  char_code(Item, Xcode),
  (
    (code_type(Item, upper), Value is Xcode - 38);  % A-Z = 27-52
    (code_type(Item, lower), Value is Xcode - 96)  % a-z = 1-26
  ).

sum_priorities(Items, Total) :-
  maplist(priority, Items, Values),
  sum_list(Values, Total).

main(File, Part1, Part2) :-
  csv_read_file(File, Rows),
  findall(X1, common_item_part1(Rows, X1), Part1Items),
  sum_priorities(Part1Items, Part1),
  findall(X2, common_item_part2(Rows, X2), Part2Items),
  sum_priorities(Part2Items, Part2).