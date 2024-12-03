-module(danila).

% https://erlangforums.com/t/advent-of-code-2024-day-2/4240/2

-compile(export_all).

main(File) ->
    {ok, RawData} = file:read_file(File),
    Data =
        [[binary_to_integer(X) || X <- binary:split(L, <<" ">>, [global, trim])]
         || L <- binary:split(RawData, <<"\n">>, [global, trim])],
    io:format("part 1: ~p~n", [solve1(Data)]),
    io:format("part 2: ~p~n", [solve2(Data)]).

solve1(Data) ->
    length([X || X <- Data, is_safe(X)]).

is_safe([H1, H2 | _] = Line) ->
    is_safe(Line, H1 < H2).

is_safe([_], _) ->
    true;
is_safe([H1, H2 | T], true = Asc) when H2 - H1 >= 1, H2 - H1 =< 3 ->
    is_safe([H2 | T], Asc);
is_safe([H1, H2 | T], false = Desc) when H1 - H2 >= 1, H1 - H2 =< 3 ->
    is_safe([H2 | T], Desc);
is_safe(_, _) ->
    false.

solve2(Data) ->
    length([X || X <- Data, lists:any(fun is_safe/1, [X | remove_one(X)])]).

remove_one(List) ->
    remove_one(List, []).

remove_one([H | T], Tail) ->
    [lists:reverse(Tail) ++ T | remove_one(T, [H | Tail])];
remove_one([], _) ->
    [].
