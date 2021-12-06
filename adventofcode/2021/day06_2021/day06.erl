-module day06.

% Problem: https://adventofcode.com/2021/day/6

% erlc day06.erl && erl -noshell -pa . -eval "day06:main()" -s init stop

-export([main/0]).

main()->
    % main("input_from_description.txt").
    main("input.txt").

main(File) ->
    {ok, RawData} = file:read_file(File),
    Data =
        [ binary_to_integer(Value)
         || Value <- re:split(RawData, "[,\n]+"),  Value =/= <<>>],
    % io:format("parsed input: ~p~n", [Data]),
    io:format("4 days: ~p~n", [part1(Data,4)]),
    % io:format("80 days: ~p~n", [part1(Data,80)]),
    % io:format("100 days: ~p~n", [part1(Data,100)]),
    io:format("120 days: ~p~n", [part1(Data,120)]).
% io:format("part1 256 days: ~p~n", [part1(Data,256)]).


part1(Data, Value) ->
    length(part1_calculate(Data, Value)).

part1_calculate(Data, 0) -> Data;
part1_calculate(Data, Value) -> 
    [Old | New] = part1_process_new(lists:map(fun act/1, Data)),
    % io:format("Old, New, Joined: ~n~p~n~p~n~p~n", [Old, New, join_lists(Old, New)]),
    part1_calculate(join_lists(Old, New), Value-1).

join_lists(First, [[]]) -> lists:reverse(First);
join_lists(First, Second) -> lists:reverse(hd(Second) ++ First).

part1_process_new(Data) ->
    lists:foldl(fun use_new/2, [[], []], Data).

use_new({Old, nothing}, [OldFish, NewFish]) ->
    [[Old | OldFish], NewFish];
use_new({Old, New}, [OldFish, NewFish]) ->
    [[Old | OldFish], [New| NewFish]].

act(Value) when Value == 0 ->
    {6, 8};
act(Value) ->
    {Value - 1, nothing}.
