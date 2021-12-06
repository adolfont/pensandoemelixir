-module day06.

% Problem: https://adventofcode.com/2021/day/6

% erlc day06.erl && erl -noshell -pa . -eval "day06:main()" -s init stop

-export([main/0]).

main()->
    main("input_from_description.txt").
    % main("input.txt").

main(File) ->
    {ok, RawData} = file:read_file(File),
    Data =
        [ binary_to_integer(Value)
         || Value <- re:split(RawData, "[,\n]+"),  Value =/= <<>>],
    % io:format("parsed input: ~p~n", [Data]),
    % io:format("4 days: ~p~n", [part1(Data,4)]),
    % io:format("80 days: ~p~n", [part1(Data,80)]),
    % io:format("100 days: ~p~n", [part1(Data,100)]),
    % io:format("120 days: ~p~n", [part1(Data,120)]).
    % 
    % 
    %
    % io:format("18 days cache: ~p~n", [part1_cache(18)]),

    % io:format("18 days (26, 1861): ~p~n", [part1(Data,18)]),
    % io:format("18 days with cache (26, 1861): ~p~n", [part1_v2(Data,18)]),
    % io:format("80 days with cache (5934, 389726): ~p~n", [part1_v2(Data,80)]),
    % io:format("120 days with cache (191336, 12181103): ~p~n", [part1_v2(Data,120)]),
    io:format("256 days with cache (26984457539, ?): ~p~n", [part1_v2(Data,256)]).
% io:format("80 days (5934, 389726): ~p~n", [part1(Data,80)]),
%   ERRO 256 deu 2309541396
    % io:format("120 days (191336, 12181103): ~p~n", [part1(Data,120)]).


part1(Data, Value) ->
    CALC = part1_calculate(lists:reverse(Data), Value),
    {CALC, length(CALC)}.

part1_v2_aux(_L, V, 0) -> V;
part1_v2_aux(L, V, N) -> 
   {L1, V1} = part1_v2(L, 64),
   part1_v2_aux(L1, V+V1, N-1).

part1_v2(Data, 256) ->
    {V1, L1} = part1_v2(Data, 64),
    {_L, V} = part1_v2_aux(L1,V1,1),
    V; 


part1_v2(Data, Count) ->
    Map = part1_cache(Count), 
    lists:foldl(fun(X, {ValueAccum, ListAccum}) -> 
       {List, Value} = maps:get(X, Map),  
       {Value + ValueAccum, List++ ListAccum } end, 
       {0, []}, Data).
part1_cache(Value) ->
    maps:from_list([{Digit, part1([Digit], Value)} || Digit <- lists:seq(0,8)]).

part1_calculate(Data, 0) -> Data;
part1_calculate(Data, Value) -> 
    [Old | New] = part1_process_new(lists:map(fun act/1, Data)),
    % io:format("Old, New, Joined: ~n~p~n~p~n~p~n", [Old, New, join_lists(Old, New)]),
    part1_calculate(join_lists(Old, New), Value-1).

join_lists(First, [[]]) -> First;
join_lists(First, Second) -> hd(Second) ++ First.

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
