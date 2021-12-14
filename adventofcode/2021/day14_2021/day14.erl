-module(day14).

-include_lib("eunit/include/eunit.hrl").

% Problem: https://adventofcode.com/2021/day/14
%
% erlc -DTEST day14.erl && erl -noshell -pa . -eval "eunit:test(day14, [verbose])" -s init stop
% OR
% erlc day14.erl && erl -noshell -pa . -eval "day14:main()" -s init stop
%

% learned: begin end in list comprehension

-export([main/0]).

-compile(export_all).

-ifdef(EUNIT).

apply_rule_test() ->
    RuleMap = maps:from_list([{"NN", "C"}]),
    ?assertEqual("C", apply_rule("NN", RuleMap)),
    ?assertEqual("", apply_rule("NC", RuleMap)).

parse_test_file_test() ->
    {Template, Rules} = parse_file(file:read_file("parse_test.txt")),
    ?assertEqual("NNCB", Template),
    ?assertEqual([{"CH", "B"}, {"HH", "N"}], Rules).

one_step_test() ->
    {Template, Rules} = parse_file(file:read_file("input_from_description.txt")),
    ?assertEqual("NCNBCHB", one_step(Template, Rules)).

one_step_twice_test() ->
    {Template, Rules} = parse_file(file:read_file("input_from_description.txt")),
    ?assertEqual("NBCCNBBBCBHCB", one_step(one_step(Template, Rules), Rules)).

four_steps_test() ->
    {Template, Rules} = parse_file(file:read_file("input_from_description.txt")),
    ?assertEqual("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB",
                 steps(4, Template, Rules)).

count_elements_test() ->
    ?assertEqual(#{"N" => 2, "X" => 1}, count_elements("XNN")).

part1_test() ->
    File = "input_from_description.txt",
    Data = parse_file(file:read_file(File)),

    ?assertEqual(1588, part1(Data)).

part1_final_test() ->
    File = "input.txt",
    Data = parse_file(file:read_file(File)),

    ?assertEqual(2967, part1(Data)).

-endif.

part1({Template, Rules}) ->
    Values = maps:values(count_elements(steps(10, Template, Rules))),
    lists:max(Values) - lists:min(Values).

count_elements(String) ->
    count_elements(String, #{}).

count_elements([], Map) ->
    Map;
count_elements([Head | Tail], Map) ->
    Fun = fun(V) -> V + 1 end,
    count_elements(Tail, maps:update_with([Head], Fun, 1, Map)).

steps(0, Template, _Rules) ->
    Template;
steps(N, Template, Rules) ->
    NewString = one_step(Template, Rules),
    steps(N - 1, NewString, Rules).

one_step(Template, Rules) ->
    one_step(Template, maps:from_list(Rules), "").

one_step([One], _RulesMap, Current) ->
    Current ++ [One];
one_step([First, Second | Tail], RulesMap, Current) ->
    Current
    ++ [First]
    ++ apply_rule([First, Second], RulesMap)
    ++ one_step([Second | Tail], RulesMap, Current).

apply_rule(Text, Map) ->
    case maps:get(Text, Map, 0) of
        0 ->
            "";
        Value ->
            Value
    end.

main() ->
    main("input_from_description.txt"),
    main("input.txt").

main(File) ->
    Data = parse_file(file:read_file(File)),
    io:format("Part 1 (1588, 2967) for ~p: ~p~n", [File, part1(Data)]).

    % io:format("Part 2 for ~p: ~p~n", [File, part2(File)]).

parse_file({ok, RawData}) ->
    [Template | Rules] = re:split(RawData, "[\n\n]"),
    {unicode:characters_to_list(Template), parse_rules(Rules)};
parse_file({error, _}) ->
    "No file with that name!".

parse_rules(List) ->
    [parse_rule(Rule) || Rule <- List, Rule =/= <<>>].

parse_rule(Rule) ->
    [Left, Right | _Tail] = re:split(Rule, "[\->]+"),
    {string:trim(
         unicode:characters_to_list(Left)),
     string:trim(
         unicode:characters_to_list(Right))}.
