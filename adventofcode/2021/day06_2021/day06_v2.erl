-module(day06_v2).

% Problem: https://adventofcode.com/2021/day/6

% erlc day06_v2.erl && erl -noshell -pa . -eval "day06_v2:main()" -s init stop

-export([main/0]).

main() ->
  main("input_from_description.txt"),

  main("input.txt").

main(File) ->
  {ok, RawData} = file:read_file(File),
  Data = [binary_to_integer(Value) || Value <- re:split(RawData, "[,\n]+"), Value =/= <<>>],
  io:format("Part 1 and 2: ~p,  ~p~n", [counter(Data, 18), counter(Data, 256)]).

counter(Data, Days) ->
  NewMap = counter_map(create_map(Data), Days),
  lists:sum(
    maps:values(NewMap)).

counter_map(Map, 0) ->
  Map;
counter_map(Map, Days) ->
  counter_map(update_map(Map), Days - 1).

update_map(Map) ->
  update_map(Map, maps:keys(Map)).

update_map(OriginalMap, Keys) ->
  XL =
    lists:sort(
      lists:flatten([update_map_based_on(X, OriginalMap) || X <- Keys])),
  maps:from_list(sum(XL)).

sum(L) ->
  Keys =
    maps:keys(
      maps:from_list(L)),
  sum(L, Keys, []).

sum(_L, [], Result) ->
  Result;
sum(L, [Key | OtherKeys], Result) ->
  sum(L,
      OtherKeys,
      [sum_same_key(Key, lists:filter(fun({X, _}) -> X == Key end, L)) | Result]).

sum_same_key(Key, L) ->
  lists:foldl(fun({X, Value1}, {X, Value2}) -> {X, Value1 + Value2} end, {Key, 0}, L).

update_map_based_on(0, OriginalMap) ->
  [{0, 0}, {6, maps:get(0, OriginalMap, 0)}, {8, maps:get(0, OriginalMap, 0)}];
update_map_based_on(N, OriginalMap) ->
  M = [{N - 1, maps:get(N, OriginalMap, 0)}, {N, 0}],
  M.

create_map(Data) ->
  lists:foldl(fun update_counter/2, #{}, Data).

update_counter(Coord, Acc) ->
  Fun = fun(V) -> V + 1 end,
  maps:update_with(Coord, Fun, 1, Acc).
