-module(danilagamma).

% Source: https://erlangforums.com/t/advent-of-code-2021-day-5/738/2
% Author of the code: Danila Gamma
% Comments by me (Adolfo Neto)
-compile(export_all).

% Comments for main/1:
% If the file name (File) is "input_from_description.txt"
% First it reads the file and puts a long binary it into RawData.
% The it splits that binary into several lines (breaking on newline \n)
% removing whitespaces befora  and after each string (trim):
% 8> binary:split(RawData, <<"\n">>, [global, trim]).
% [<<"0,9 -> 5,9">>,<<"8,0 -> 0,8">>,<<"9,4 -> 3,4">>,
%  <<"2,2 -> 2,1">>,<<"7,0 -> 7,4">>,<<"6,4 -> 2,0">>,
%  <<"0,9 -> 2,9">>,<<"3,4 -> 1,4">>,<<"0,0 -> 8,8">>,
%  <<"5,5 -> 8,2">>]
%
% Them for each Line such as <<"0,9 -> 5,9">>
%
% 9> Line = <<"0,9 -> 5,9">>.
% <<"0,9 -> 5,9">>
%
% First it splits it into two parts separated by " -> "
% 10> [From, To] = binary:split(Line, <<" -> ">>).
% [<<"0,9">>,<<"5,9">>]
% Then, from the resulting list with two binaries (From and To),
% the code gets the binary to the left and the binary to the right of the comma:
% 11> [X1b, Y1b] = binary:split(From, <<",">>).
% [<<"0">>,<<"9">>]
% 12> [X2b, Y2b] = binary:split(To,   <<",">>).
% [<<"5">>,<<"9">>]
%
% Finally, it converts each binary to an integer:
% 13> {{binary_to_integer(X1b), binary_to_integer(Y1b)},
% 13>                   {binary_to_integer(X2b), binary_to_integer(Y2b)}}.
% {{0,9},{5,9}}
%
% And the result is a tuple with two tuples, each tuple containing two integers.
%
% The whole process:
%
% Eshell V12.1  (abort with ^G)
% 1> {ok, RawData} = file:read_file("input_from_description.txt").
% {ok,<<"0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,"...>>}
% 2> Data = [ begin
% 2>                  [From, To] = binary:split(Line, <<" -> ">>),
% 2>                  [X1b, Y1b] = binary:split(From, <<",">>),
% 2>                  [X2b, Y2b] = binary:split(To,   <<",">>),
% 2>                  {{binary_to_integer(X1b), binary_to_integer(Y1b)},
% 2>                   {binary_to_integer(X2b), binary_to_integer(Y2b)}}
% 2>              end || Line <- binary:split(RawData, <<"\n">>, [global, trim]) ].
% [{{0,9},{5,9}},
%  {{8,0},{0,8}},
%  {{9,4},{3,4}},
%  {{2,2},{2,1}},
%  {{7,0},{7,4}},
%  {{6,4},{2,0}},
%  {{0,9},{2,9}},
%  {{3,4},{1,4}},
%  {{0,0},{8,8}},
%  {{5,5},{8,2}}]

% Beautiful!
% I learned that you can use begin and end on list comprehensions.

main(File) ->
    {ok, RawData} = file:read_file(File),
    Data =
        [begin
             [From, To] = binary:split(Line, <<" -> ">>),
             [X1b, Y1b] = binary:split(From, <<",">>),
             [X2b, Y2b] = binary:split(To, <<",">>),
             {{binary_to_integer(X1b), binary_to_integer(Y1b)},
              {binary_to_integer(X2b), binary_to_integer(Y2b)}}
         end
         || Line <- binary:split(RawData, <<"\n">>, [global, trim])],
    io:format("part 1: ~p~n", [solve1(Data)]),
    io:format("part 2: ~p~n", [solve2(Data)]),
    ok.

% To understand the argument to count_overlap/1,
% I had to try this in erl:
% 30> G = fun(Z)->[Z*2] end.
% #Fun<erl_eval.44.65746770>
% 31> [ X || L1 <- [1,2,3], X <- G(L1)].
% [2,4,6]
% So, for each item (two points) in Data, the list comprehension replaces it
% with a list containing the points of the straight line
% between those two points.
%
solve1(Data) ->
    count_overlap([L || Coords <- Data, L <- to_straight_line(Coords)]).

% Suppose Data is
% Data =
% [{{0,9},{5,9}},
%  {{8,0},{0,8}},
%  {{9,4},{3,4}},
%  {{2,2},{2,1}},
%  {{7,0},{7,4}},
%  {{6,4},{2,0}},
%  {{0,9},{2,9}},
%  {{3,4},{1,4}},
%  {{0,0},{8,8}},
%  {{5,5},{8,2}}].
% Then:
% Arg = [ L || Coords <- Data, L <- to_straight_line(Coords) ].
% is
% 7> Arg = [ L || Coords <- Data, L <- danilagamma:to_straight_line(Coords) ].
% [{0,9},
%      {1,9},
%      {2,9},
% ...
%      {1,4}]
% What does count_overlap/1 do?

% > danilagamma:count_overlap(Arg).
% 5
%
% The lists:foldl counts how much of each pair there is on the list of pairs (points).
% It starts with an empty map #{}.
% 9> lists:foldl(fun danilagamma:update_counter/2, #{}, Arg).
% #{{0,9} => 2,
%   {1,4} => 1,
%   {1,9} => 2,
%   {2,1} => 1,
%   ...
%   {8,4} => 1,
%   {9,4} => 1}
%
% The second line gets only the values of the map
% and filters those whose are equal or superior to 2.
% I believe an alternative implementation of
% [ X || X <- maps:values(Map), X >= 2 ]
% would be
% lists:filter(fun(X) -> X>=2 end, maps:values(Map)).
% Then it gets the length of that list, that is,
% the number of points that appear twice or more in the original list.
count_overlap(Lines) ->
    Map = lists:foldl(fun update_counter/2, #{}, Lines),
    length([X || X <- maps:values(Map), X >= 2]).

% Just updates a map
% If the key is not there, it will be added with one.
% If the key is there, its value will be increased by one.
update_counter(Coord, Acc) ->
    Fun = fun(V) -> V + 1 end,
    maps:update_with(Coord, Fun, 1, Acc).

% This one is quite easy to understand and the implementation is good:
% 33> danilagamma:to_straight_line({{1,1}, {1,8}}).
% [{1,1},{1,2},{1,3},{1,4},{1,5},{1,6},{1,7},{1,8}]
% I just refactored it to use pattern matching.
to_straight_line({{X, Y1}, {X, Y2}}) ->
    [{X, Y} || Y <- from_to(Y1, Y2)];
to_straight_line({{X1, Y}, {X2, Y}}) ->
    [{X, Y} || X <- from_to(X1, X2)];
to_straight_line(_) ->
    [].

from_to(A, B) ->
    case B > A of
        true ->
            lists:seq(A, B);
        false ->
            lists:seq(A, B, -1)
    end.

% Wow! Very good!
% It just added
% ++ to_diagonal_line(Coords)
% and then it was done!
solve2(Data) ->
    count_overlap([L
                   || Coords <- Data, L <- to_straight_line(Coords) ++ to_diagonal_line(Coords)]).

% to_diagonal_line does quite a simple job!
% 2> danilagamma:to_diagonal_line({{1,1},{3,3}}).
% [{1,1},{2,2},{3,3}]
% 3> danilagamma:to_diagonal_line({{3,3},{1,1}}).
% [{3,3},{2,2},{1,1}]
% 4> danilagamma:to_diagonal_line({{1,3},{3,1}}).
% [{1,3},{2,2},{3,1}]
% 5> danilagamma:to_diagonal_line({{3,1},{1,3}}).
% [{3,1},{2,2},{1,3}]
% 6> danilagamma:to_diagonal_line({{3,1},{1,4}}).
% []
% But the way lists:zip/2 was used with from_to/2 was great!
to_diagonal_line({{X1, Y1}, {X2, Y2}}) when abs(X1 - X2) =:= abs(Y1 - Y2) ->
    lists:zip(from_to(X1, X2), from_to(Y1, Y2));
to_diagonal_line(_) ->
    [].
