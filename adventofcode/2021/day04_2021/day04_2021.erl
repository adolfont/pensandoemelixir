-module(day04_2021).

% See tests down below!
-include_lib("eunit/include/eunit.hrl").

% erlc -DTEST day04_2021.erl && erl -noshell -pa . -eval "eunit:test(day04_2021, [verbose])" -s init stop

-export([find_bingo_number_2/2, read_input/1, sum_unmarked/2]).

%%% HELPER FUNCTIONS FOR BOTH PARTS

mark_number(Map = #{numbers := []}) ->
    Map;
mark_number(Map = #{numbers := [Head | Tail], matrices := Matrices}) ->
    Map#{numbers => Tail, matrices => [mark_number(Head, Matrix) || Matrix <- Matrices]}.

mark_number(Number, Matrix) ->
    [mark_line(Number, Line) || Line <- Matrix].

mark_line(Number, Line) ->
    lists:map(fun(X) -> mark(X, Number) end, Line).

mark(X, X) ->
    {X, marked};
mark(X, _) ->
    X.

bingo(Matrix) ->
    bingo([{true, clean(Line)} || Line <- Matrix, bingo_line(Line)], Matrix).

bingo([{true, Line} | _], Matrix) ->
    {true, Line, Matrix};
bingo([], Matrix) ->
    case [{true, clean(Line)} || Line <- transpose(Matrix), bingo_line(Line)] of
        [] ->
            false;
        [{true, Line} | _] ->
            {true, Line, Matrix}
    end.

clean(Line) ->
    lists:map(fun({X, _}) -> X end, Line).

bingo_line(Line) ->
    lists:all(fun(X) -> is_tuple(X) end, Line).

%%% PART 1

find_bingo_number(Map = #{numbers := [LatestNumber | _]}) ->
    NewMap = mark_number(Map),
    #{matrices := Matrices} = NewMap,
    find_bingo_number(lists:filter(fun(X) -> X =/= false end,
                                   [bingo(Matrix) || Matrix <- Matrices]),
                      NewMap,
                      LatestNumber).

find_bingo_number([], NewMap, _) ->
    find_bingo_number(NewMap);
find_bingo_number([{true, Line, Matrix} | _], _, LatestNumber) ->
    {LatestNumber, Line, Matrix}.

part1(Map) ->
    {Number, _List, Matrix} = find_bingo_number(Map),
    Number * sum_unmarked(Matrix, 0).

sum_unmarked([], Sum) ->
    Sum;
sum_unmarked([Head | Tail], Sum) ->
    sum_unmarked(Tail, Sum + sum_unmarked_line(Head)).

sum_unmarked_line(Line) ->
    lists:sum(
        lists:filter(fun is_integer/1, Line)).

%%% PART 2

% I wrote the following comments to try to understand what I was doing.
% I am not sure they reflect the code.
%
% find_bingo_number_2/2
% the first argument is the map with numbers and boards (matrices)
% the second argument is the current list of solutions
% 1. it marks the first number on all boards
% 2. it gets the Matrices from the new map
% 3. it calls find_bingo_number_2_aux/4 with
%   a) a list containing only false or a list containing a line/row that allows you to say bingo!
%   b) the NewMap
%   c) the latest number used (which is not on NewMap anymore)
%   d) an empty list of solutions
find_bingo_number_2(Map = #{numbers := [LatestNumber | _]}, Solutions) ->
    NewMap = mark_number(Map),
    #{matrices := Matrices} = NewMap,
    find_bingo_number_2_aux(lists:filter(fun(X) -> X =/= false end,
                                         [bingo(Matrix) || Matrix <- Matrices]),
                            NewMap,
                            LatestNumber,
                            Solutions);
find_bingo_number_2(_Map, Solutions) ->
    Solutions.

% find_bingo_number_2_aux/4
% this is the stop clause
% when there are no more matrices, you return the solutions
find_bingo_number_2_aux(_, #{matrices := []}, _, Solutions) ->
    Solutions;
% this is the start/continue clause
% the first argument are the new solutions that allow you to say bingo!
% the second argument is the current map
% the third argument is the latest number that created the solutions
% the fourth argument are the current solutions
% if I have solutions,
% I remove the matrices(boards) that were solved from the Matrices
% I add the new solutions to the current solutions
% And I call find_bingo_number_2_aux/4 recursively
% with the NewMap
find_bingo_number_2_aux(Solutions,
                        Map = #{matrices := Matrices},
                        LatestNumber,
                        CurSolutions)
    when [] =/= Solutions ->
    GetMatrix = fun({_, _, X}) -> X end,
    MatricesToDelete = [GetMatrix(S) || S <- Solutions],
    GetSolution = fun(Y) -> {LatestNumber, Y} end,
    SolutionsToAdd = [GetSolution(S) || S <- Solutions],
    NewMap = Map#{matrices => lists:subtract(Matrices, MatricesToDelete)},
    find_bingo_number_2(NewMap, SolutionsToAdd ++ CurSolutions);
find_bingo_number_2_aux([], Map, _LatestNumber, Solutions) ->
    find_bingo_number_2(Map, Solutions).

part2(Map) ->
    {LatestNumber, {true, _, Matrix}} = hd(find_bingo_number_2(Map, [])),
    LatestNumber * sum_unmarked(Matrix, 0).

%% GENERAL HELPER FUNCTIONS

% Source: https://stackoverflow.com/questions/5389254/transposing-a-2-dimensional-matrix-in-erlang
transpose([[] | _]) ->
    [];
transpose(M) ->
    [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

input(InputFileName) ->
    {ok, InputFromFile} = file:read_file(InputFileName),
    string:trim(
        unicode:characters_to_list(InputFromFile)).

read_numbers_line(String, Separator) ->
    [list_to_integer(S) || S <- string:split(String, [Separator], all), S =/= ""].

read_matrix(String) ->
    lists:map(fun(S) -> read_numbers_line(S, " ") end, string:split(String, [$\n], all)).

read_input(InputFileName) ->
    PartsOfTheInput = [S || S <- string:split(input(InputFileName), "\n\n", all), S =/= ""],
    [NumbersLine | Matrices] = PartsOfTheInput,
    #{numbers => read_numbers_line(NumbersLine, $,),
      matrices => lists:map(fun read_matrix/1, Matrices)}.

%%% TESTS

-ifdef(EUNIT).

read_numbers_line_test() ->
    ?assertEqual([1, 2, 3], read_numbers_line("1,2,3", $,)).

read_matrix_test() ->
    ?assertEqual([[1, 2, 3], [4, 5, 6]], read_matrix("1 2 3\n 4 5 6")).

read_input_test() ->
    ?assertEqual(#{matrices => [[[22, 13], [1, 2]], [[3, 15], [9, 18]]],
                   numbers => [7, 4, 9]},
                 read_input("test_input.txt")).

mark_test() ->
    ?assertEqual({1, marked}, mark(1, 1)),
    ?assertEqual(1, mark(1, 2)).

mark_number_test() ->
    ?assertEqual([22, {30, marked}], mark_line(30, [22, 30])),
    ?assertEqual([[22, {30, marked}], [{30, marked}, 2]],
                 mark_number(30, [[22, 30], [30, 2]])),

    ?assertEqual(#{matrices => [[[22, 13], [1, 2]], [[3, 15], [{9, marked}, 18]]],
                   numbers => []},
                 mark_number(mark_number(mark_number(read_input("test_input.txt"))))).

bingo_test() ->
    % base: [[1,2], [3,4]]
    ?assertEqual(true, bingo_line([{1, marked}, {2, marked}])),
    Input1 = [[{1, marked}, {2, marked}], [3, 4]],
    ?assertEqual({true, [1, 2], Input1}, bingo(Input1)),
    Input2 = [[1, 2], [{3, marked}, {4, marked}]],
    ?assertEqual({true, [3, 4], Input2}, bingo(Input2)),
    Transposed = transpose([[{1, marked}, 2], [{3, marked}, 4]]),
    ?assertEqual(true, bingo_line(hd(Transposed))),
    ?assertEqual({true, [1, 3], Transposed}, bingo(Transposed)),
    Input4 = [[{1, marked}, 2], [{3, marked}, 4]],
    ?assertEqual({true, [1, 3], Input4}, bingo(Input4)),
    Input5 = [[1, {2, marked}], [3, {4, marked}]],
    ?assertEqual({true, [2, 4], Input5}, bingo(Input5)).

find_bingo_number_test() ->
    ?assertEqual({4, [2, 4], [[1, {2, marked}], [3, {4, marked}]]},
                 find_bingo_number(read_input("test_input_2.txt"))),
    {Number, Line, _Matrix} = find_bingo_number(read_input("input_from_description.txt")),
    ?assertEqual({24, [14, 21, 17, 24, 4]}, {Number, Line}).

part1_test() ->
    ?assertEqual(16, part1(read_input("test_input_3.txt"))),
    ?assertEqual(14, part1(read_input("test_input_4.txt"))),
    ?assertEqual(4512, part1(read_input("input_from_description.txt"))),
    ?assertEqual(64084, part1(read_input("input.txt"))).

find_bingo_number_2_test() ->
    ?assertEqual({1, {true, [1, 2], [[22, 13], [{1, marked}, {2, marked}]]}},
                 hd(find_bingo_number_2(read_input("test_input_3.txt"), []))).

part2_test() ->
    ?assertEqual(35, part2(read_input("test_input_3.txt"))),
    ?assertEqual(230, part2(read_input("test_input_4.txt"))),

    ?assertEqual(1924, part2(read_input("input_from_description.txt"))),
    ?assertEqual(12833, part2(read_input("input.txt"))).

-endif.
