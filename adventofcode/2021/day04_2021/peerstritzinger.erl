-module(peerstritzinger).

%Source: https://erlangforums.com/t/advent-of-code-2021-day-4/734/9

-export([p4/0]).

p4() ->
    io:format("~p~n", [p4("test_input_3.txt")]),
    io:format("~p~n", [p4("test_input_4.txt")]),
    io:format("~p~n", [p4("input_from_description.txt")]),
    io:format("~p~n", [p4("input.txt")]).

p4(FileName) ->
    [Nums, Boards] = p4_read(FileName),
    Tab = lists:sort([{steps(Nums, B ++ transpose(B), 0), B} || B <- Boards]),
    {{_, L1, S1}, _} = hd(Tab),
    {{_, L2, S2}, _} = lists:last(Tab),
    {L1 * (S1 div 2), L2 * (S2 div 2)}.

steps([], _, Count) ->
    {Count + 1, [], 0};
steps([Num | R], Board, Count) ->
    B = play(Num, Board),
    case bingo(B) of
        true ->
            {Count + 1,
             Num,
             lists:sum(
                 lists:flatten(B))};
        false ->
            steps(R, B, Count + 1)
    end.

play(Num, Board) ->
    [[E || E <- L, E =/= Num] || L <- Board].

bingo(Board) ->
    lists:any(fun(X) -> X =:= [] end, Board).

p4_read(Fn) ->
    {ok, Bin} = file:read_file(Fn),
    [Nums | Boards] =
        string:split(
            string:trim(Bin), "\n\n", all),
    deep_integer([string:split(Nums, ",", all),
                  [[string:lexemes(L, " ") || L <- string:split(B, "\n", all)] || B <- Boards]]).

deep_integer(L) when is_list(L) ->
    [deep_integer(E) || E <- L];
deep_integer(B) when is_binary(B) ->
    binary_to_integer(B).

transpose([[] | _]) ->
    [];
transpose(LoL) ->
    R = [hd(L) || L <- LoL],
    Cs = [tl(L) || L <- LoL],
    [R | transpose(Cs)].
