-module(day09).

-include_lib("eunit/include/eunit.hrl").

% Problem: https://adventofcode.com/2021/day/8
%
% erlc -DTEST day09.erl && erl -noshell -pa . -eval "eunit:test(day09, [verbose])" -s init stop
% OR
% erlc day09.erl && erl -noshell -pa . -eval "day09:main()" -s init stop
%

% learned: begin end in list comprehension

-export([main/0]).

-ifdef(EUNIT).

-endif.

main() ->
    main("input_from_description.txt"),
    main("input.txt").

main(File) ->
    Data = parse_file(file:read_file(File)),

    io:format("Part 1 (9, 514) for ~p: ~p~n", [File, part1(Data)]),
    io:format("Part 2 (1134, 1103130) for ~p: ~p~n", [File, part2(Data)]).

part2(Data) ->
    Basins = [lists:sort(X) || X <- find_three_largest_basins(Data)],
    [B1, B2, B3 | _] =
        lists:sort(fun(X, Y) -> length(X) > length(Y) end, remove_dups(Basins)),
    LBasins = [length(X) || X <- [B1, B2, B3]],
    lists:foldl(fun(X, Y) -> X * Y end, 1, LBasins).

remove_dups([]) ->
    [];
remove_dups([H | T]) ->
    [H | [X || X <- remove_dups(T), X /= H]].

find_three_largest_basins(Data) ->
    find_three_largest_basins(create_map(Data), length(Data), length(hd(Data)), []).

find_three_largest_basins(Map, Lines, Columns, _ThreeLargest) ->
    [basin_size({Line, Column}, Map, [])
     || Line <- lists:seq(1, Lines), Column <- lists:seq(1, Columns)].

basin_size({Line, Column}, Map, Basin) ->
    Value = maps:get({Line, Column}, Map, $9),
    case (Value =/= $9) and not lists:member({Line, Column}, Basin) of
        true ->
            basin_size_rec({Line, Column}, Map, [{Line, Column} | Basin]);
        false ->
            Basin
    end.

basin_size_rec({Line, Column}, Map, Basin) ->
    B2 = basin_size({Line - 1, Column}, Map, Basin),
    B3 = basin_size({Line + 1, Column}, Map, B2),
    B4 = basin_size({Line, Column - 1}, Map, B3),
    B5 = basin_size({Line, Column + 1}, Map, B4),
    B5.

part1(Data) ->
    LowPoints = find_low_points(Data),
    lists:sum([X - 48 + 1 || X <- LowPoints]).

find_low_points(Data) ->
    find_low_points(create_map(Data), length(Data), length(hd(Data))).

find_low_points(Map, Lines, Columns) ->
    [maps:get({Line, Column}, Map)
     || Line <- lists:seq(1, Lines),
        Column <- lists:seq(1, Columns),
        is_low_point(Map, Line, Column)].

is_low_point(Map, Line, Column) ->
    Value = maps:get({Line, Column}, Map),
    (Value < maps:get({Line - 1, Column}, Map, 100))
    and (Value < maps:get({Line + 1, Column}, Map, 100))
    and (Value < maps:get({Line, Column - 1}, Map, 100))
    and (Value < maps:get({Line, Column + 1}, Map, 100)).

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

parse_file({ok, RawData}) ->
    [unicode:characters_to_list(Line) || Line <- re:split(RawData, "[\n]+"), Line =/= <<>>];
parse_file({error, _}) ->
    "No file with that name!".
