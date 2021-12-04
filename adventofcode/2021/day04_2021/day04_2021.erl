-module(day04_2021).

% See tests down below!
-include_lib("eunit/include/eunit.hrl").

% erlc -DTEST day04_2021.erl && erl -noshell -pa . -eval "eunit:test(day04_2021, [verbose])" -s init stop

-export([transpose/1]).

%%% HELPER FUNCTIONS

% parse(String) ->
%     [command(S) || S <- string:split(String, [$\n], all), S =/= ""].

% command(S) ->
%     string:trim(S).

% processed_input() ->
%     parse(input()).

% processed_input(InputFileName) ->
%     parse(input(InputFileName)).

%% PART 1

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

%% HELPER FUNCTIONS

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

-ifdef(EUNIT).

read_numbers_line_test() ->
    ?assertEqual([1, 2, 3], read_numbers_line("1,2,3", $,)).

read_matrix_test() ->
    ?assertEqual([[1, 2, 3], [4, 5, 6]], read_matrix("1 2 3\n 4 5 6")).

read_input_test() ->
    ?assertEqual(#{matrices => [[[22, 13], [1, 2]], [[3, 15], [9, 18]]],
                   numbers => [7, 4, 9]},
                 read_input("test_input.txt")).

    % ?assertEqual(#{}, read_input("input_from_description.txt")).

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
    ?assertEqual(4512, part1(read_input("input_from_description.txt"))),
    ?assertEqual(64084, part1(read_input("input.txt"))).

-endif.
