-module(day07).

% Problem: https://adventofcode.com/2021/day/7

% erlc day07.erl && erl -noshell -pa . -eval "day07:main()" -s init stop

-export([main/0]).

main() ->
  main("input_from_description.txt"),

  main("input.txt").

main(File) ->
  Data = parse_file(file:read_file(File)),

  % io:format("Part 1 parsed: ~p~n", [Data]),
  io:format("Part 1 for ~p: ~p~n", [File, part1(Data)]),
  io:format("Part 2 for ~p: ~p~n", [File, part2(Data)]).

max_value(List) ->
  lists:max(List).

min_value(List) ->
  lists:min(List).

part1(Data) ->
  part_gen(1, Data).

part2(Data) ->
  part_gen(2, Data).

part_gen(1, Data) ->
  part_gen(fun position_fuel/2, Data);
part_gen(2, Data) ->
  part_gen(fun position_fuel_2/2, Data);
part_gen(F, Data) ->
  {_, Result} =
    lists:foldl(fun(Position, Accum) -> choose_best_fuel(F, Position, Accum, Data) end,
                {-1, -1},
                lists:seq(min_value(Data), max_value(Data))),
  Result.

choose_best_fuel(F, Position, {-1, -1}, Data) ->
  F(Position, Data);
choose_best_fuel(F, Position, Current, Data) ->
  {P1, F1} = F(Position, Data),
  {_PA, FA} = Current,
  if F1 < FA ->
       {P1, F1};
     true ->
       Current
  end.

position_fuel(Position, Data) ->
  {Position, lists:foldl(fun(X, Accum) -> Accum + abs(Position - X) end, 0, Data)}.

position_fuel_2(Position, Data) ->
  {Position,
   lists:foldl(fun(X, Accum) -> Accum + fuel_consumption(Position, X) end, 0, Data)}.

fuel_consumption(Position1, Position2) when Position1 > Position2 ->
  fuel_consumption(Position2, Position1);
fuel_consumption(Position1, Position2) ->
  fuel_consumption(Position1, Position2, 0, 1).

fuel_consumption(Position, Position, Total, _Counter) ->
  Total;
fuel_consumption(Position1, Position2, Total, Counter) ->
  fuel_consumption(Position1 + 1, Position2, Total + Counter, Counter + 1).

parse_file({ok, RawData}) ->
  [binary_to_integer(Value) || Value <- re:split(RawData, "[,\n]+"), Value =/= <<>>];
parse_file({error, _}) ->
  "No file with that name!".
