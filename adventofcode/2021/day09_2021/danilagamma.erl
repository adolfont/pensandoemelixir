-module(danilagamma).

% Author: Danila Fediashchin @danilagamma
% Source: https://erlangforums.com/t/advent-of-code-2021-day-9/773/2?u=adolfont

% How to run it:
% erlc danilagamma.erl && erl -noshell -pa . -eval "danilagamma:main()" -s init stop
-export([main/0]).


main() ->
    main("input_from_description.txt"),
    main("input.txt").

main(File) ->
    {ok, RawData} = file:read_file(File),
    Data = [ [ N - $0 || N <- binary_to_list(Line) ]
             || Line <- binary:split(RawData, <<"\n">>, [global, trim]) ],
    Map = load(Data),
    io:format("part 1: ~p~n", [solve1(Map)]),
    io:format("part 2: ~p~n", [solve2(Map)]).

load(Data) ->
    maps:from_list([ {{X, Y}, N} || {Y, Line} <- enum(Data),
                                    {X, N}    <- enum(Line) ]).

enum(List) ->
    lists:zip(lists:seq(1, length(List)), List).

solve1(Map) ->
    maps:fold(fun (XY, V, Acc) ->
                  case is_low_point(XY, V, Map) of
                      true  -> 1 + V + Acc;
                      false -> Acc
                  end
              end, 0, Map).

is_low_point(XY, Value, Map) ->
    Adjacent = [ maps:get(A, Map) || A <- adjacent_coords(XY),
                                     maps:is_key(A, Map) ],
    lists:all(fun (AdjValue) -> AdjValue > Value end, Adjacent).

adjacent_coords({X, Y}) ->
    [{X - 1, Y},
     {X + 1, Y},
     {X, Y - 1},
     {X, Y + 1}].

solve2(Map) ->
    Basins = maps:fold(fun (XY, V, Acc) ->
                           case is_low_point(XY, V, Map) of
                               true  -> [basin(XY, V, #{}, Map)|Acc];
                               false -> Acc
                           end
                       end, [], Map),
    lists:foldl(fun erlang:'*'/2, 1,
                [ maps:size(X) || X <- lists:sublist(lists:reverse(lists:sort(Basins)), 3) ]).

basin(XY0, V0, Acc0, Map) ->
    ToFollow = [ {A, N} || A <- adjacent_coords(XY0),
                               maps:is_key(A, Map),
                           not maps:is_key(A, Acc0),
                           N <- [maps:get(A, Map)],
                           N >   V0,
                           N =/= 9 ],
    lists:foldl(fun ({XY, V}, Acc) -> basin(XY, V, Acc, Map) end, Acc0#{XY0 => V0}, ToFollow).