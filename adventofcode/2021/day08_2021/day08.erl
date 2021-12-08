-module(day08).

-include_lib("eunit/include/eunit.hrl").

% Problem: https://adventofcode.com/2021/day/8
%
% erlc -DTEST day08.erl && erl -noshell -pa . -eval "eunit:test(day08, [verbose])" -s init stop
% OR
% erlc day08.erl && erl -noshell -pa . -eval "day08:main()" -s init stop
% 

THIS PROGRAM DOES NOT SOLVE THE PROBLEM! 

-export([main/0]).

-ifdef(EUNIT).

digits_test() ->
  ?assertEqual(6, num_digits(0)),
  ?assertEqual(2, num_digits(1)),
  ?assertEqual(5, num_digits(2)),
  ?assertEqual(5, num_digits(3)),
  ?assertEqual(4, num_digits(4)),
  ?assertEqual(5, num_digits(5)),
  ?assertEqual(6, num_digits(6)),
  ?assertEqual(3, num_digits(7)),
  ?assertEqual(7, num_digits(8)),
  ?assertEqual(6, num_digits(9)).

mapping_test() ->
  Input =
    ["acedgfb", "cdfbe", "gcdfa", "fbcad", "dab", "cefabd", "cdfgeb", "eafb", "cagedb", "ab"],
  Map = map_from_input(Input),
  ?assertEqual(0, Map).

-endif.

map_from_input(List) ->
  % io:format("Before sort ~p~n", [List]),
  L = sort_input(List),
  % io:format("~p~n", [L]),
  map_from_input(L, #{}).

map_from_input([], CurrentMap) ->
  CurrentMap;
map_from_input([Head | Tail], CurrentMap) ->
  case length(maps:keys(CurrentMap)) of
    10 ->
      CurrentMap;
    _ ->
      map_from_input(Tail, add(Head, CurrentMap))
  end.

sort_input(List) ->
    Length_sorter = fun (X,Y) -> length(X) < length(Y) end,

    {Left, Right} =
    lists:partition(fun(X) ->
                  lists:member(length(X), [2,3,4,7])
               end,
               List),
    lists:sort(Length_sorter, Left) ++
    lists:sort(Length_sorter, Right).

add(Str, CurrentMap) ->
  PossibleDigits = maps:get(length(Str), reverse_num_digits()),
  case is_integer(PossibleDigits) of
    true ->
      new_map_with(PossibleDigits, Str, CurrentMap);
    false ->
      CurrentMap
  end.

new_map_with(Digit, String, CurrentMap) when is_integer(Digit) ->
  % [io:format("~p ", [Char]) || Char <- String],
  io:format("~p -- ", [maps:get(Digit, digits())]),
  io:format("~p~n", [String]),
  NewMapItens =
  [
    {[X], [String| maps:get([X], CurrentMap, [])]}

  || X <- maps:get(Digit, digits())

  ],

  io:format("NewMapItens: ~p -- ~n", 
  [ 
    NewMapItens
  ]),

  lists:foldl(
      fun({X,Y}, Acc) -> 
      Acc#{X => Y ++ maps:get([X],CurrentMap,[])  } end, CurrentMap, NewMapItens
  );
  % lists:foldl(
  %   fun(X, Acc) -> Acc#{X => X} end, CurrentMap, String
  % ),
  % CurrentMap;
new_map_with(_, _, CurrentMap) ->
  CurrentMap.

main() ->
  main("input_from_description.txt"),
  main("input.txt").

num_digits(N) ->
  lists:sum(
    maps:get(N, digits())).

hardcoded_num_digits() ->
  #{0 => 6,
    1 => 2,
    2 => 5,
    3 => 5,
    4 => 4,
    5 => 5,
    6 => 6,
    7 => 3,
    8 => 7,
    9 => 6}.

reverse_num_digits() ->
  #{2 => 1,
    3 => 7,
    4 => 4,
    5 => [2, 3, 5],
    6 => [0, 6, 9],
    7 => 8}.

digits() ->
  #{% a b c d e f g
    0 => "abcefg",
    1 => "cf",
    2 => "acdeg",
    3 => "acdfg",
    4 => "cbdf",
    5 => "abdfg",
    6 => "abcdfg",
    7 => "acf",
    8 => "abcdefg",
    9 => "abcdfg"}.

  % digits() ->
  %   #{% a b c d e f g
  %     0 => [1, 1, 1, 0, 1, 1, 1],
  %     1 => [0, 0, 1, 0, 0, 1, 0],
  %     2 => [1, 0, 1, 1, 1, 0, 1],
  %     3 => [1, 0, 1, 1, 0, 1, 1],
  %     4 => [0, 1, 1, 1, 0, 1, 0],
  %     5 => [1, 1, 0, 1, 0, 1, 1],
  %     6 => [1, 1, 0, 1, 1, 1, 1],
  %     7 => [1, 0, 1, 0, 0, 1, 0],
  %     8 => [1, 1, 1, 1, 1, 1, 1],
  %     9 => [1, 1, 1, 1, 0, 1, 1]}.

main(File) ->
  Data = parse_file(file:read_file(File)),

  % io:format("Part 1 parsed: ~p~n", [Data]),
  io:format("Part 1 for ~p: ~p~n",
            [File, part1(Data)]).  % io:format("Part 2 for ~p: ~p~n", [File, part2(Data)]).

part1(Data) ->
  lists:foldl(fun part1_counter/2, 0, Data).

part1_counter({_Left, Right}, Value) ->
  Value
  + length(lists:filter(fun(X) ->
                           (length(X) == 2)
                           or (length(X) == 3)
                           or (length(X) == 4)
                           or (length(X) == 7)
                        end,
                        Right)).

parse_file({ok, RawData}) ->
  [parse_line(Line) || Line <- re:split(RawData, "[\n]+"), Line =/= <<>>];
parse_file({error, _}) ->
  "No file with that name!".

parse_line(Line) ->
  % io:format("~p~n", [unicode:characters_to_list(Line)]),
  [Left, Right | _Tail] = re:split(Line, "[|]+"),
  % io:format("~p~n", [Left]),
  % io:format("~p~n", [Right]),
  LeftStrings =
    [unicode:characters_to_list(Value) || Value <- re:split(Left, "[ ]+"), Value =/= <<>>],
  RightStrings =
    [unicode:characters_to_list(Value) || Value <- re:split(Right, "[ ]+"), Value =/= <<>>],
  {LeftStrings, RightStrings}.
