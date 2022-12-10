calorie_sums([Counts|Rest], Calories) :-
  atomic_list_concat(FoodStrCounts, '\n', Counts),
  maplist(atom_number, FoodStrCounts, FoodCounts),
  sum_list(FoodCounts, Calories);
  calorie_sums(Rest, Calories).

get_calories(File, CaloriesList) :-
  open(File, read, Stream),
  read_string(Stream, _, String),
  atomic_list_concat(Counts, '\n\n', String),
  bagof(X, calorie_sums(Counts, X), CaloriesList).

part1(File, MaxCalories) :-
  get_calories(File, List),
  max_list(List, MaxCalories).

part2(File, Max3Calories) :-
  get_calories(File, List),
  sort(0, @>=, List, [C1,C2,C3|_]),
  sum_list([C1,C2,C3], Max3Calories).
