-module(day02_2021_v2).

-export([main/0]).

% erlc day02_2021_v2.erl && cat input.txt | erl -noshell -s day02_2021_v2 main

% See "erl -noshell" here https://www.erlang.org/faq/how_do_i.html#idm600
main() ->
    {Part1, Part2} = process(0, 0, 0, 0),
    io:format("Part 1: ~p~n", [Part1]),
    io:format("Part 2: ~p~n", [Part2]),
    init:stop().

process(H, V, V2, A) ->
    case io:get_line('') of
        eof ->
            {H * V, H * V2};
        Text ->
            process(Text, H, V, V2, A)
    end.

process(String, H, V, V2, A) ->
    case string:split(String, " ") of
        ["forward", Value] ->
            process(H + int(Value), V, V2 + A * int(Value), A);
        ["down", Value] ->
            process(H, V + int(Value), V2, A + int(Value));
        ["up", Value] ->
            process(H, V - int(Value), V2, A - int(Value))
    end.

% Auxiliary functions

int(S) ->
    {Value, _} = string:to_integer(S),
    Value.
