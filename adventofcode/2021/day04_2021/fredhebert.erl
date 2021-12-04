-module(fredhebert).
-export([main/0]).

% Source https://erlangforums.com/t/advent-of-code-2021-day-4/734/5
% 
% This only solves Part 1

main() ->
    main(["input_from_description.txt"]),
    main(["input.txt"]).

main([File]) ->
    {ok,Bin} = file:read_file(File),
    [RawDraws|Rest] = string:split(Bin, "\n\n", leading),
    Draws = [binary_to_integer(B) || B <- string:split(RawDraws, ",", all)],
    Boards = [[binary_to_integer(RawNum)
               || RawNum <- re:split(RawBoard, "\\s+", [multiline]),
                  RawNum =/= <<>>]
              || RawBoard <- string:split(Rest, "\n\n", all)],
    io:format("~p~n", [play(Draws, Boards)]).

win(L) ->
    lists:any(fun(SL) -> length(SL)==5 end,
              [lists:filter(fun(X) -> X==x end, lists:sublist(L, N, 5))
               || N <- [1,6,11,16,21]])
    orelse
    lists:any(fun(SL) -> length(SL)==5 end,
              [lists:filter(fun(X) -> X==x end,
                            [lists:nth(N,L), lists:nth(N+5,L), lists:nth(N+10,L),
                             lists:nth(N+15,L), lists:nth(N+20,L)])
               || N <- [1,2,3,4,5]]).

mark([],_) -> [];
mark([X|T],X) -> [x|T];
mark([H|T],X) -> [H|mark(T,X)].

score(L) ->
    lists:sum([X || X <- L, X=/=x]).

play([D|Ds], Boards) ->
    NewBoards = [mark(B, D) || B <- Boards],
    case [B || B <- NewBoards, win(B)] of
        [] -> play(Ds, NewBoards);
        [W] -> score(W)*D
    end.