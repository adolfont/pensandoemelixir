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

-compile(export_all).

-ifdef(EUNIT).

-endif.

main() ->
    main("input_from_description.txt"),

    main("input.txt").

main(File) ->
    Data = parse_file(file:read_file(File)),

    io:format("Part 1 for ~p: ~p~n", [File, part1(Data)]).

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
