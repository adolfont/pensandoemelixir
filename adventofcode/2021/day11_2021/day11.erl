-module(day11).

-include_lib("eunit/include/eunit.hrl").

% Problem: https://adventofcode.com/2021/day/11
%
% erlc -DTEST day11.erl && erl -noshell -pa . -eval "eunit:test(day11, [verbose])" -s init stop
% OR
% erlc day11.erl && erl -noshell -pa . -eval "day11:main()" -s init stop
%

% learned: nothing new today

-export([main/0]).

-ifdef(EUNIT).

-endif.

main() ->
  % main("input_from_description_2.txt"),
  main("input_from_description.txt").

  % main("input.txt").

main(File) ->
  Data = parse_file(file:read_file(File)),
  Steps = 3,
  io:format("Part 1 after ~p steps (1656, ?) for ~p: ~p~n",
            [Steps, File, count_flashes(Data, Steps)]).

  % io:format("Part 2 (288957, 2870201088) for ~p: ~p~n", [File, part2(Data)]).

count_flashes(Data, MaxSteps) ->
  Lines = length(Data),
  Columns = length(hd(Data)),
  Map = create_map(Data),
  io:fwrite("Initial Map~n"),
  show_map(Map, Lines, Columns),
  count_flashes(Map, Lines, Columns, 0, 0, MaxSteps).

count_flashes(Map, Lines, Columns, MaxSteps, Flashes, MaxSteps) ->
  io:fwrite("Final Map~n"),
  show_map(Map, Lines, Columns),
  Flashes;
count_flashes(Map, Lines, Columns, Steps, Flashes, MaxSteps) ->
  NewMap = plus_one(Map, Lines, Columns),
  io:fwrite("After plus one~n"),
  show_map(NewMap, Lines, Columns),
  {NewFlashes, MapAfterFlashes} = propagate_flash(NewMap, Lines, Columns),
  io:fwrite("After propagating flashes~n"),
  show_map(MapAfterFlashes, Lines, Columns),
  count_flashes(MapAfterFlashes, Lines, Columns, Steps + 1, Flashes + NewFlashes, MaxSteps).

propagate_flash(Map, Lines, Columns) ->
  InitialFlashes =
    [{X, Y}
     || X <- lists:seq(1, Lines), Y <- lists:seq(1, Columns), maps:get({X, Y}, Map) > $9],
  update_map_with_flashes(InitialFlashes, Map, 0, Lines, Columns).

update_map_with_flashes([], Map, FlashCounter, Lines, Columns) ->
  {FlashCounter, Map},
  M2 = clean(Map),
  io:fwrite("After Clean~n"),
  show_map(M2, Lines, Columns),
  {FlashCounter, M2};
update_map_with_flashes([Head | Tail], Map, FlashCounter, Lines, Columns) ->
  update_map_with_flashes(Tail,
                          update_map_based_on(Head, Map, Lines, Columns),
                          FlashCounter + 1,
                          Lines,
                          Columns).

clean(Map) ->
  maps:from_list([{{X, Y},
                   if (V > $9) ->
                        V - 10;
                      (V =< $9) ->
                        V
                   end}
                  || {{X, Y}, V} <- maps:to_list(Map)]).

update_map_based_on(Head, Map, Lines, Columns) ->
  {X, Y} = Head,
  Value = maps:get({X, Y}, Map),
  update_neighbors(neighbors(Head), Map#{Head := Value - 10}, Lines, Columns).

update_neighbors([], Map, _Lines, _Columns) ->
  Map;
update_neighbors([Head | Tail], Map, Lines, Columns) ->
  update_neighbors(Tail, new_value(Map, Head, Lines, Columns), Lines, Columns).

new_value(Map, {X, Y}, Lines, Columns)
  when (X < 1) or (Y < 1) or (X > Lines) or (Y > Columns) ->
  Map;
new_value(Map, {X, Y}, _L, _C) ->
  Map#{{X, Y} := maps:get({X, Y}, Map) + 1}.

neighbors({X, Y}) ->
  [{X - 1, Y - 1},
   {X - 1, Y},
   {X - 1, Y + 1},
   {X, Y - 1},
   {X, Y + 1},
   {X + 1, Y - 1},
   {X + 1, Y},
   {X + 1, Y + 1}].

plus_one(Map, Lines, Columns) ->
  maps:from_list([{{X, Y}, maps:get({X, Y}, Map) + 1}
                  || X <- lists:seq(1, Lines), Y <- lists:seq(1, Columns)]).

% create_map/1 comes from Day 09.

create_map(Data) ->
  create_map(Data, #{}, 1).

create_map([], Map, _) ->
  Map;
create_map([Head | Tail], Map, X) ->
  create_map(Tail, create_map_from_line(Head, Map, X), X + 1).

create_map_from_line(Line, Map, X) ->
  {_, NewMap} =
    lists:foldl(fun(Height, {Y, M}) -> {Y + 1, M#{{X, Y} => Height}} end, {1, Map}, Line),
  NewMap.

%%% HELPER FUNCTIONS

show_map(Map, Lines, Columns) ->
  [io:format("~p~n", [show_line(Map, X, Columns)]) || X <- lists:seq(1, Lines)],
  io:fwrite("~n").

show_line(Map, X, Columns) ->
  [maps:get({X, Y}, Map) || Y <- lists:seq(1, Columns)].

parse_file({ok, RawData}) ->
  [unicode:characters_to_list(Line) || Line <- re:split(RawData, "[\n]+"), Line =/= <<>>];
parse_file({error, _}) ->
  "No file with that name!".
