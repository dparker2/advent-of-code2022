score(rock, 1).
score(paper, 2).
score(scissors, 3).
score(loss, 0).
score(draw, 3).
score(win, 6).

beats(rock, scissors).
beats(scissors, paper).
beats(paper, rock).

outcome(Opp, Me, Outcome) :- Opp = Me, Outcome = draw.
outcome(Opp, Me, Outcome) :- beats(Opp, Me), Outcome = loss.
outcome(Opp, Me, Outcome) :- beats(Me, Opp), Outcome = win.

move("A", rock).
move("X", rock).
move("B", paper).
move("Y", paper).
move("C", scissors).
move("Z", scissors).

file_line(File, Line) :-
  open(File, read, In),
  repeat,
    (   read_line_to_string(In, Line0),
        Line0 \== end_of_file
    ->  Line0 = Line
    ;   !,
        fail
    ).

game_score(File, Score, Mover) :-
  file_line(File, Line),
  split_string(Line, " ", "", [Opp, Me]),
  move(Opp, OppMove), call(Mover, Opp, Me, MyMove),
  outcome(OppMove, MyMove, Outcome),
  score(MyMove, X), score(Outcome, Y),
  Score is X + Y.

part1_mover(_, Me, MyMove) :- move(Me, MyMove).

play_part1(File, TotalScore) :-
  findall(Score, game_score(File, Score, part1_mover), Scores),
  sum_list(Scores, TotalScore).

part2_mover(Opp, "X", MyMove) :- move(Opp, OppMove), outcome(OppMove, MyMove, loss).
part2_mover(Opp, "Y", MyMove) :- move(Opp, OppMove), outcome(OppMove, MyMove, draw).
part2_mover(Opp, "Z", MyMove) :- move(Opp, OppMove), outcome(OppMove, MyMove, win).

play_part2(File, TotalScore) :-
  findall(Score, game_score(File, Score, part2_mover), Scores),
  sum_list(Scores, TotalScore).
