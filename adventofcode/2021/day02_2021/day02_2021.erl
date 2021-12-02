-module(day02_2021).

-include_lib("eunit/include/eunit.hrl").

-export([solve_part1/1, solve_part2/1, input/0, input/1]).

% erlc -DTEST day02_2021.erl && erl -noshell -pa . -eval "eunit:test(day02_2021, [verbose])" -s init stop

solve_part1(Input) ->
    solve_part1(parse(Input), 0, 0).

solve_part1([], H, V) ->
    H * V;
solve_part1([{forward, X} | Tail], H, V) ->
    solve_part1(Tail, H + X, V);
solve_part1([{up, X} | Tail], H, V) ->
    solve_part1(Tail, H, V - X);
solve_part1([{down, X} | Tail], H, V) ->
    solve_part1(Tail, H, V + X).

solve_part2(Input) ->
    solve_part2(parse(Input), 0, 0, 0).

solve_part2([], H, V, _A) ->
    H * V;
solve_part2([Head | Tail], H, V, A) ->
    case Head of
        {forward, X} ->
            solve_part2(Tail, H + X, V + A * X, A);
        {up, X} ->
            solve_part2(Tail, H, V, A - X);
        {down, X} ->
            solve_part2(Tail, H, V, A + X)
    end.

parse(String) ->
    [command(S) || S <- string:split(String, [$\n], all), S =/= ""].

int(S) ->
    {Value, _} = string:to_integer(S),
    Value.

command(S) ->
    case string:split(S, " ") of
        ["forward", X] ->
            {forward, int(X)};
        ["up", X] ->
            {up, int(X)};
        ["down", X] ->
            {down, int(X)}
    end.

input() ->
    "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2".

input(InputFileName) ->
    {ok, InputFromFile} = file:read_file(InputFileName),
    unicode:characters_to_list(InputFromFile).

-ifdef(EUNIT).

part1_test() ->
    ?assertEqual(150, solve_part1(input())),
    ?assertEqual(1654760, solve_part1(input("input.txt"))).

part2_test() ->
    ?assertEqual(900, solve_part2(input())),
    ?assertEqual(1956047400, solve_part2(input("input.txt"))).

-endif.
