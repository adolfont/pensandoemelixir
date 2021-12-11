-module(day10).

-include_lib("eunit/include/eunit.hrl").

% Problem: https://adventofcode.com/2021/day/10
%
% erlc -DTEST day10.erl && erl -noshell -pa . -eval "eunit:test(day10, [verbose])" -s init stop
% OR
% erlc day10.erl && erl -noshell -pa . -eval "day10:main()" -s init stop
%

% learned: nothing new today

-export([main/0]).

-ifdef(EUNIT).

-endif.

main() ->
  main("input_from_description.txt"),

  main("input.txt").

main(File) ->
  Data = parse_file(file:read_file(File)),

  io:format("Part 1 (26397, 364389) for ~p: ~p~n", [File, part1(Data)]),

  io:format("Part 2 (288957, 2870201088) for ~p: ~p~n", [File, part2(Data)]).

part1(Data) ->
  lists:sum([parse_line(Line) || Line <- Data, is_integer(parse_line(Line))]).

part2(Data) ->
  get_middle_score([score(parse_line(Line))
                    || Line <- Data, not is_integer(parse_line(Line))]).

get_middle_score(List) ->
  lists:nth(1 + length(List) div 2, lists:sort(List)).

% The score is determined by considering the completion string character-by-character.
% Start with a total score of 0.
% Then, for each character, multiply the total score by 5 and
% then increase the total score by the point value given for the character
% in the following table:
% ): 1 point.
% ]: 2 points.
% }: 3 points.
% >: 4 points.
score(Line) ->
  lists:foldl(fun(X, Acc) -> 5 * Acc + points(X) end, 0, Line).

points($() ->
  1;
points($[) ->
  2;
points(${) ->
  3;
points($<) ->
  4.

parse_line(String) ->
  parse_line(String, []).

parse_line([], OpenChars) when is_integer(OpenChars) ->
  OpenChars;
parse_line([], OpenChars) ->
  OpenChars;
parse_line([Head | Tail], OpenChars) ->
  parse_line_char(Head, Tail, OpenChars).

parse_line_char($(, Tail, OpenChars) ->
  parse_line(Tail, [$( | OpenChars]);
parse_line_char($[, Tail, OpenChars) ->
  parse_line(Tail, [$[ | OpenChars]);
parse_line_char(${, Tail, OpenChars) ->
  parse_line(Tail, [${ | OpenChars]);
parse_line_char($<, Tail, OpenChars) ->
  parse_line(Tail, [$< | OpenChars]);
parse_line_char($), Tail, [$( | OpenChars]) ->
  parse_line(Tail, OpenChars);
parse_line_char($], Tail, [$[ | OpenChars]) ->
  parse_line(Tail, OpenChars);
parse_line_char($}, Tail, [${ | OpenChars]) ->
  parse_line(Tail, OpenChars);
parse_line_char($>, Tail, [$< | OpenChars]) ->
  parse_line(Tail, OpenChars);
% Par ): 3 points.
% Bra ]: 57 points.
% Cur }: 1197 points.
% Bigger >: 25137 points.
parse_line_char($), _Tail, _) ->
  parse_line([], 3);
parse_line_char($], _Tail, _) ->
  parse_line([], 57);
parse_line_char($}, _Tail, _) ->
  parse_line([], 1197);
parse_line_char($>, _Tail, _) ->
  parse_line([], 25137).

parse_file({ok, RawData}) ->
  [unicode:characters_to_list(Line) || Line <- re:split(RawData, "[\n]+"), Line =/= <<>>];
parse_file({error, _}) ->
  "No file with that name!".
