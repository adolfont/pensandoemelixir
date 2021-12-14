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
                 steps(4, fun one_step_part2/2, Template, Rules)).

count_elements_test() ->
    ?assertEqual(#{"N" => 2, "X" => 1}, count_elements("XNN")),
    ?assertEqual(#{"N" => 5, "B" => 11, "C"=>5, "H"=>4}, 
        count_elements("NBBBCNCCNBBNBNBBCHBHHBCHB")).

part1_test() ->
    File = "input_from_description.txt",
    Data = parse_file(file:read_file(File)),

    ?assertEqual(1588, part1(Data)).

part1_final_test() ->
    File = "input.txt",
    Data = parse_file(file:read_file(File)),

    ?assertEqual(2967, part1(Data)).

make_pairs_test() ->
    ?assertEqual(["AB", "BC"], make_pairs("ABC")),
    ?assertEqual(["AB", "BC", "CD", "DE"], make_pairs("ABCDE")).

one_step_part2_test() ->
    File = "input_from_description.txt",
    {Template, Rules} = parse_file(file:read_file(File)),
    ?assertEqual("NCNBCHB", one_step_part2(Template, Rules)).

count_letters_test() ->
    M = #{"AC" => 3,"BC" => 1,"CA" => 2,"CB" => 1,"CD" => 1,"D" => 1},

    ?assertEqual(#{"A" => 3,"B" => 1,"C" => 4,"D" => 1}, 
        count_letters(M)).

 part2_test() ->
     File = "input_from_description.txt",
     Data = parse_file(file:read_file(File)),
     ?assertEqual(2188189693529, part2(Data)).

-endif.

main() ->
    main("input_from_description.txt"),
    main("input.txt").

main(File) ->
    Data = parse_file(file:read_file(File)),
    io:format("Part 1 (1588, 2967) for ~p: ~p~n", [File, part1(Data)]),

    io:format("Part 2 (2188189693529, 3692219987038) for ~p: ~n~p~n", [File, part2(Data)]).

part1(Data) ->
    part2(Data, 10).
    % Values = maps:values(count_elements(steps(3, fun one_step_part2/2, Template, Rules))),
    % lists:max(Values) - lists:min(Values).

part2(Data) -> part2(Data, 40).

part2({Template, Rules}, Steps) ->
    RulesMap = maps:from_list(Rules),
    TemplatePairs = make_pairs_or_ones(Template),
    TemplateFrequencies = create_map_frequencies(TemplatePairs),
    Values = maps:values(count_letters(steps_2(Steps, TemplateFrequencies, RulesMap))),
    lists:max(Values) - lists:min(Values).

steps_2(0, Template, _Rules) ->
    Template;
steps_2(N, Template, Rules) ->
    steps_2(N - 1, step(Template, Rules), Rules).

step(TemplateFrequencies, RulesMap) -> 
    M = lists:foldl(
        fun (Substr, AccumList) ->
          [update_frequencies(Substr, TemplateFrequencies, RulesMap)|AccumList] end, 
         [], maps:keys(TemplateFrequencies)),
    M1 = lists:flatten(M),
    TF = update_with(TemplateFrequencies, M1 ),
    TF.

update_with(Map, Pairs) -> 
    lists:foldl(fun({Chars, IncDec}, AccumMap) ->
        AccumMap#{Chars => maps:get(Chars, AccumMap,0)+IncDec} end, Map, Pairs).
count_letters(Map) ->
    lists:foldl(
        fun({Expr, Value}, AccumMap) ->
            sum_values(Expr, Value, AccumMap) end,
         #{}, maps:to_list(Map)).
sum_values([Char], Value, AccumMap) -> 
    CurValue = maps:get([Char], AccumMap, 0),
    AccumMap#{[Char] => CurValue + Value};
sum_values([Char1, _Char2], Value, AccumMap) ->
    sum_values([Char1], Value, AccumMap).




update_frequencies([_Char], _FreqMap, _RulesMap) -> []; 

update_frequencies(Substr = [Char1, Char2], FreqMap, RulesMap) -> 
    case maps:get(Substr, RulesMap, -1) of
        -1 ->
            [];
        Char3 ->
            update_frequencies([Char1], [Char2], Char3, FreqMap)
    end.

update_frequencies(Char, Char, Char, AccumMap) -> 
    % AA -> A, logo AA -> AAA
   AA = Char ++ Char,
   NumAA = maps:get(AA, AccumMap, 0),
  [{AA, NumAA}];

update_frequencies(Char, Char2, Char, AccumMap) ->
    % AB -> A, logo AB -> AAB; para cada AB, um novo AA
   AA = Char ++ Char,
   AB = Char ++ Char2,
   NumAB = maps:get(AB, AccumMap, 0),
  [{AA, NumAB}];

update_frequencies(Char2, Char, Char, AccumMap) ->
    % BA -> A, logo BA -> BAA; para cada BA, um novo AA 
   AA = Char ++ Char,
   BA = Char2 ++ Char,
   NumBA = maps:get(BA, AccumMap, 0),
  [{AA, NumBA}];

update_frequencies(Char1, Char2, Char3, AccumMap) -> 
   % Rule: XY -> Z; XY dá origem a XZ e YZ, some XY
    XY = Char1 ++ Char2,
    XZ = Char1 ++ Char3,
    YZ = Char3 ++ Char2,

    NumXY = maps:get(XY, AccumMap, 0),
    [{XY, -NumXY}, {XZ, NumXY}, {YZ, NumXY}].

create_map_frequencies(List) ->
    lists:foldl(fun frequency/2, #{}, List). 

frequency(Key, Map) ->
    Map#{Key => maps:get(Key, Map, 0)+1}.

make_pairs_or_ones(List) ->
    make_pairs_or_ones(List, []).

make_pairs_or_ones([Char], List) ->
    [[Char] | List];
make_pairs_or_ones([Char1, Char2 | Rest], List) ->
    make_pairs_or_ones([Char2 | Rest], [[Char1, Char2] | List]).

one_step_part2(Template, Rules) ->
    TemplatePairs = make_pairs(Template),
    RulesMap = maps:from_list(Rules),
    OnePass = lists:map(fun(X) -> apply_rule(X, RulesMap) end, TemplatePairs),
    lists:flatten(
        lists:zipwith(fun(X, Y) -> X ++ Y end, make_ones(Template), OnePass ++ [""])).

count_elements(String) ->
    count_elements(String, #{}).

count_elements([], Map) ->
    Map;
count_elements([Head | Tail], Map) ->
    Fun = fun(V) -> V + 1 end,
    count_elements(Tail, maps:update_with([Head], Fun, 1, Map)).

steps(0, _, Template, _Rules) ->
    Template;
steps(N, StepFunction, Template, Rules) ->
    NewString = StepFunction(Template, Rules),
    steps(N - 1, StepFunction, NewString, Rules).

one_step(Template, Rules) ->
    one_step(Template, maps:from_list(Rules), "").

one_step([One], _RulesMap, Current) ->
    Current ++ [One];
one_step([First, Second | Tail], RulesMap, Current) ->
    Current
    ++ [First]
    ++ apply_rule([First, Second], RulesMap)
    ++ one_step([Second | Tail], RulesMap, Current).

make_ones(List) ->
    [[X] || X <- List].

make_pairs(List) ->
    lists:reverse(make_pairs(List, [])).

make_pairs([Char1, Char2], List) ->
    [[Char1, Char2] | List];
make_pairs([Char1, Char2 | Rest], List) ->
    make_pairs([Char2 | Rest], [[Char1, Char2] | List]).

apply_rule(Text, Map) ->
    case maps:get(Text, Map, 0) of
        0 ->
            "";
        Value ->
            Value
    end.


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
