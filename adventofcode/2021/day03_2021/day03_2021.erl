-module(day03_2021).

-include_lib("eunit/include/eunit.hrl").

% erlc -DTEST day03_2021.erl && erl -noshell -pa . -eval "eunit:test(day03_2021, [verbose])" -s init stop

-export([solve_part1/1, solve_part2/1]).

% PART 1
solve_part1(List) ->
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

% PART 2
%
solve_part2(List) ->
    list_to_integer(calc_O2_rating(List), 2) * list_to_integer(calc_scruber_rating(List), 2).

calc_O2_rating(List) ->
    calc_O2_rating(List, 1).

calc_O2_rating([OneValue], _Index) ->
    OneValue;
calc_O2_rating(List, Index) ->
    % divides into two groups
    {StartsWithZero, StartsWithOne} =
        lists:partition(fun(Str) -> lists:nth(Index, Str) == hd("0") end, List),
    calc_O2_rating(next_list(StartsWithZero, StartsWithOne), Index + 1).

calc_scruber_rating(List) ->
    calc_scruber_rating(List, 1).

calc_scruber_rating([OneValue], _Index) ->
    OneValue;
calc_scruber_rating(List, Index) ->
    % divides into two groups
    {StartsWithZero, StartsWithOne} =
        lists:partition(fun(Str) -> lists:nth(Index, Str) == hd("0") end, List),
    calc_scruber_rating(next_list_v2(StartsWithZero, StartsWithOne), Index + 1).

next_list(List1 = [_], []) ->
    List1;
next_list([], List2 = [_]) ->
    List2;
next_list(List1, List2) when length(List1) > length(List2) ->
    List1;
next_list(_List1, List2) ->
    List2.

next_list_v2(List1 = [_], []) ->
    List1;
next_list_v2([], List2 = [_]) ->
    List2;
next_list_v2(List1, List2) when length(List2) < length(List1) ->
    List2;
next_list_v2(List1, _List2) ->
    List1.

%%% HELPER FUNCTIONS

parse(String) ->
    [command(S) || S <- string:split(String, [$\n], all), S =/= ""].

command(S) ->
    string:trim(S).

processed_input() ->
    parse(input()).

processed_input(InputFileName) ->
    parse(input(InputFileName)).

input() ->
    "00100\n    11110\n    10110\n    10111\n    10101\n    01111\n "
    "   00111\n    11100\n    10000\n    11001\n    00010\n    01010\n".

input(InputFileName) ->
    {ok, InputFromFile} = file:read_file(InputFileName),
    unicode:characters_to_list(InputFromFile).

%%% TESTS

-ifdef(EUNIT).

part1_test() ->
    ?assertEqual("11011", command("11011")),
    ?assertEqual(["00100",
                  "11110",
                  "10110",
                  "10111",
                  "10101",
                  "01111",
                  "00111",
                  "11100",
                  "10000",
                  "11001",
                  "00010",
                  "01010"],
                 processed_input()),
    ?assertEqual(6, calculate_numbers(fun bool/1, [true, true, false])),
    ?assertEqual([0, 0, 0], zero_counter("111", [0, 0, 0])),
    ?assertEqual([0, 1, 0], zero_counter("101", [0, 0, 0])),
    ?assertEqual(198, solve_part1(processed_input())),
    ?assertEqual(1092896, solve_part1(processed_input("input.txt"))).

part2_test() ->
    ?assertEqual("10111", calc_O2_rating(processed_input())),
    ?assertEqual("01010", calc_scruber_rating(processed_input())),
    ?assertEqual(230, solve_part2(processed_input())),
    ?assertEqual(4672151, solve_part2(processed_input("input.txt"))).

-endif.
