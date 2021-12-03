-module(day03_2021).

-include_lib("eunit/include/eunit.hrl").

% erlc -DTEST day03_2021.erl && erl -noshell -pa . -eval "eunit:test(day03_2021, [verbose])" -s init stop

-export([solve_part1/1, input/0, zero_counter/2, parse/1, calculate_numbers/2]).

solve_part1(String) ->
    List = parse(String),
    Acum = lists:duplicate(length(hd(List)), 0),
    Size = length(List),
    Zeroes = lists:foldl(fun zero_counter/2, Acum, List),
    MostCommon = lists:map(fun(X) -> X >= Size div 2 end, Zeroes),
    calculate_numbers(fun bool/1, MostCommon)
    * calculate_numbers(fun opposite_bool/1, MostCommon).

bool(true) ->
    1;
bool(false) ->
    0.

opposite_bool(X) ->
    1 - bool(X).

calculate_numbers(F, MostCommon) ->
    Expo = length(MostCommon) - 1,
    {Result, _} =
        lists:foldl(fun(Bool, {Sum, Counter}) ->
                       {Sum + F(Bool) * math:pow(2, Counter), Counter - 1}
                    end,
                    {0, Expo},
                    MostCommon),
    trunc(Result).

zero_counter(String, Accumulator) ->
    Count = lists:map(fun(X) -> 49 - X end, String),
    lists:map(fun({X, Y}) -> X + Y end, lists:zip(Count, Accumulator)).

parse(String) ->
    [command(S) || S <- string:split(String, [$\n], all), S =/= ""].

% int(S) ->
%     {Value, _} = string:to_integer(S),
%     Value.

command(S) ->
    string:trim(S).

input() ->
    "00100\n    11110\n    10110\n    10111\n    10101\n    01111\n "
    "   00111\n    11100\n    10000\n    11001\n    00010\n    01010\n".

input(InputFileName) ->
    {ok, InputFromFile} = file:read_file(InputFileName),
    unicode:characters_to_list(InputFromFile).

-ifdef(EUNIT).

part1_test() ->
    ?assertEqual("11011", command("11011")),
    ?assertEqual(["00100","11110","10110","10111","10101","01111","00111",
    "11100","10000", "11001", "00010", "01010"], parse(input())),
    ?assertEqual([0, 0, 0], zero_counter("111", [0, 0, 0])),
    ?assertEqual([0, 1, 0], zero_counter("101", [0, 0, 0])),
    ?assertEqual(198, solve_part1(input())),
    ?assertEqual(1092896, solve_part1(input("input.txt"))).

% part2_test() ->
%     ?assertEqual(900, solve_part2(input())),
%     ?assertEqual(1956047400, solve_part2(input("input.txt"))).

-endif.
