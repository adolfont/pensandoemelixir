-module(day08).

-include_lib("eunit/include/eunit.hrl").

% Problem: https://adventofcode.com/2021/day/8
%
% erlc -DTEST day08.erl && erl -noshell -pa . -eval "eunit:test(day08, [verbose])" -s init stop
% OR
% erlc day08.erl && erl -noshell -pa . -eval "day08:main()" -s init stop
% 

-export([main/0]).

-ifdef(EUNIT).


-endif.


main() ->
  main("input_from_description.txt"),
  main("input.txt").

main(File) ->
  Data = parse_file(file:read_file(File)),

  io:format("Part 1 for ~p: ~p~n",
            [File, part1(Data)]),
  io:format("Part 2 for ~p: ~p~n", [File, part2(File)]).


part1(Data) ->
  lists:foldl(fun part1_counter/2, 0, Data).


part1_counter({_Left, Right}, Value) ->
  Value
  + length(lists:filter(fun(X) ->
                           (length(X) == 2)
                           or (length(X) == 3)
                           or (length(X) == 4)
                           or (length(X) == 7)
                        end,
                        Right)).

parse_file({ok, RawData}) ->
  [parse_line(Line) || Line <- re:split(RawData, "[\n]+"), Line =/= <<>>];
parse_file({error, _}) ->
  "No file with that name!".

parse_line(Line) ->
  % io:format("~p~n", [unicode:characters_to_list(Line)]),
  [Left, Right | _Tail] = re:split(Line, "[|]+"),
  % io:format("~p~n", [Left]),
  % io:format("~p~n", [Right]),
  LeftStrings =
    [unicode:characters_to_list(Value) || Value <- re:split(Left, "[ ]+"), Value =/= <<>>],
  RightStrings =
    [unicode:characters_to_list(Value) || Value <- re:split(Right, "[ ]+"), Value =/= <<>>],
  {LeftStrings, RightStrings}.


  % Code by Peer
  % Source https://erlangforums.com/t/advent-of-code-2021-day-8/766/5
part2(Filename) ->
  lists:sum([ digit_solve(A,B) || {A,B}<- p8_read(Filename) ]).


digit_solve(A,B) ->
    L = [ {P, [ S || S <- lists:seq(0,9), length(segs(S)) =:= length(P) ]} 
          || P <- A ],
    M = solve_steps(L),
    Res = lists:append([ element(2, lists:keyfind(D, 1, M))  ||  D <- B ]),
    list_to_integer([ R+$0 || R <- Res ]).

solve_steps(L) ->
    case lists:partition(fun({_, Ds}) -> length(Ds) =:= 1 end, L) of
        {Solved, []} -> Solved;
        {Solved, Un} -> 
            solve_steps(Solved ++ [ reduce(U, Solved) || U <- Un ])
    end.

reduce(U, Solved) ->       
    lists:foldl(fun reduce1/2, U, Solved).

reduce1({Sp, [D]},{Up, Uds}) ->     
    Diff = length(Up -- Sp),
    {Up, [ U || U <- Uds, length(segs(U) -- segs(D)) =:= Diff ]}.
     
p8_read(Fn) ->
    {ok, Bin} = file:read_file(Fn),
    Tab = [ lists:splitwith(fun(T) -> T =/= "|" end, [ canon_digit(D) 
        || D <- string:split(L, " ", all) ] ) 
      || L <- string:split(string:trim(Bin), "\n", all) ],
    [ {A, tl(B)} || {A,B} <- Tab ].
    
               
canon_digit(Bin) ->
    lists:sort([ C || <<C:8>> <= Bin ]). 

segs(0) ->
    "abcefg";
segs(1) ->
    "cf";
segs(2) ->
    "acdeg";
segs(3) ->
    "acdfg";
segs(4) ->
    "bcdf";
segs(5) ->
    "abdfg";
segs(6) ->
    "abdefg";
segs(7) ->
    "acf";
segs(8) ->
    "abcdefg";
segs(9) ->
    "abcdfg".
