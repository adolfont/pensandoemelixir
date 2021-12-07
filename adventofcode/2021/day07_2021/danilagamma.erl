-module(danilagamma).

% Author: Danila Fediashchin
% Source: https://erlangforums.com/t/advent-of-code-2021-day-7/754/2?u=adolfont
%
% I have adapted this code a bit, using my parse_file function.
% Much more efficient than my code.

-export([main/0]).

main() ->
    main("input_from_description.txt"),
    main("input.txt").



main(File) ->
    Data = parse_file(file:read_file(File)),
    io:format("part 1: ~p~n", [solve1(Data)]),
    io:format("part 2: ~p~n", [solve2(Data)]),
    ok.

parse_file({ok, RawData}) ->
    [binary_to_integer(Value) || Value <- re:split(RawData, "[,\n]+"), Value =/= <<>>];
  parse_file({error, _}) ->
    "No file with that name!".
  

solve1(Data) ->
    run(fun(X) -> X end, Data).

run(FuelFun, Data) ->
    lists:min([lists:sum([FuelFun(abs(D - N)) || D <- Data])
               || N
                      <- lists:seq(
                             lists:min(Data), lists:max(Data))]).

solve2(Data) ->
    run(fun fuel/1, Data).

fuel(X) ->
    X * (1 + X) div 2.
