-module(day05).

-export([main/0, all_pairs_2/1]).

% Problem: https://adventofcode.com/2021/day/5

% erlc day05.erl && erl -noshell -pa . -eval "day05:main()" -s init stop

main() ->
    io:fwrite("Part 1~n"),
    lazy_part_1("input_from_description.txt"),
    lazy_part_1("input.txt"),
    io:fwrite("Part 2~n"),
    lazy_part_2("input_from_description.txt"),
    lazy_part_2("input.txt").

lazy_part_1(Filename) ->
    io:format("Filename: ~p~n", [Filename]),
    % io:format("Input from file: ~p~n", [input(Filename)]),
    % io:format("Processed input from file:~n ~p~n", [processed_input(Filename)]),
    % io:format("~p~n", [all_pairs({{0,1}, {0,9}})]),
    % io:format("~p~n", [all_pairs({{0,9}, {0,1}})]),
    % io:format("~p~n", [all_pairs({{0,9}, {5,9}})]),
    % io:format("~p~n", [all_pairs({{5,9}, {0,9}})]).
    % io:format("All pairs generated from file:~n ~p~n",
    %       [generate_all_pairs(processed_input(Filename))]),
    io:format("Result: ~p~n",
              [length(generate_all_pairs(fun all_pairs/1, processed_input(Filename)))]).

lazy_part_2(Filename) ->
    io:format("Filename: ~p~n", [Filename]),
    io:format("Result: ~p~n",
              [length(generate_all_pairs(fun all_pairs_2/1, processed_input(Filename)))]).

generate_all_pairs(F, List) ->
    L = lists:flatten(
            lists:map(F, List)),
    SL = lists:sort(L),
    USL = lists:usort(L),
    lists:usort(
        lists:subtract(SL, USL)).

all_pairs({{X, Y1}, {X, Y2}}) when Y1 =< Y2 ->
    [{X, Y} || Y <- lists:seq(Y1, Y2)];
all_pairs({{X, Y1}, {X, Y2}}) ->
    [{X, Y} || Y <- lists:seq(Y2, Y1)];
all_pairs({{X1, Y}, {X2, Y}}) when X1 =< X2 ->
    [{X, Y} || X <- lists:seq(X1, X2)];
all_pairs({{X1, Y}, {X2, Y}}) ->
    [{X, Y} || X <- lists:seq(X2, X1)];
all_pairs(_) ->
    [].

all_pairs_2({{X, Y1}, {X, Y2}}) when Y1 =< Y2 ->
    [{X, Y} || Y <- lists:seq(Y1, Y2)];
all_pairs_2({{X, Y1}, {X, Y2}}) ->
    [{X, Y} || Y <- lists:seq(Y2, Y1)];
all_pairs_2({{X1, Y}, {X2, Y}}) when X1 =< X2 ->
    [{X, Y} || X <- lists:seq(X1, X2)];
all_pairs_2({{X1, Y}, {X2, Y}}) ->
    [{X, Y} || X <- lists:seq(X2, X1)];
all_pairs_2({{X1, Y1}, {X2, Y2}}) when abs(X2 - X1) == abs(Y2 - Y1) ->
    all_pairs_2_aux({{X1, Y1}, {X2, Y2}});
all_pairs_2(_) ->
    [].

all_pairs_2_aux({{X1, Y1}, {X2, Y2}}) when X1 > X2 ->
    all_pairs_2_aux({{X2, Y2}, {X1, Y1}});
all_pairs_2_aux({{X1, Y1}, {X2, Y2}}) when Y1 < Y2 ->
    [{X1 + Step, Y1 + Step} || Step <- range(0, X2 - X1)];
all_pairs_2_aux({{X1, Y1}, {X2, _Y2}}) ->
    [{X1 + Step, Y1 - Step} || Step <- range(0, X2 - X1)].

range(X, Y) when X < Y ->
    lists:seq(X, Y);
range(X, Y) ->
    lists:seq(X, Y, -1).

processed_input(InputFileName) ->
    parse(input(InputFileName)).

input(InputFileName) ->
    {ok, InputFromFile} = file:read_file(InputFileName),
    unicode:characters_to_list(InputFromFile).

parse(String) ->
    [parse_line(S) || S <- string:split(String, [$\n], all), S =/= ""].

parse_line(S) ->
    [Left, Right] =
        string:split(
            string:trim(S), "->", all),
    {parse_pair(Left), parse_pair(Right)}.

parse_pair(S) ->
    [X, Y] =
        string:split(
            string:trim(S), ",", all),
    {list_to_integer(X), list_to_integer(Y)}.
