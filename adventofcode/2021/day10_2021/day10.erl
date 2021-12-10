-module(day10).

-include_lib("eunit/include/eunit.hrl").

% Problem: https://adventofcode.com/2021/day/8
%
% erlc -DTEST day10.erl && erl -noshell -pa . -eval "eunit:test(day10, [verbose])" -s init stop
% OR
% erlc day10.erl && erl -noshell -pa . -eval "day10:main()" -s init stop
%

% learned: ???

-export([main/0]).

-ifdef(EUNIT).

-endif.

main() ->
  main("input_from_description.txt"),

   main("input.txt").

main(File) ->
  Data = parse_file(file:read_file(File)),

  io:format("Part 1 (26397, 364389) for ~p: ~p~n", [File, part1(Data)]).

% io:format("Part 2 (1134, 1103130) for ~p: ~p~n", [File, part2(Data)]).

part1(Data) ->
  lists:sum([parse_line(Line) || Line <- Data]).

parse_line(String) ->
  parse_line(String, []).


parse_line([], OpenChars) when is_integer(OpenChars) ->
  OpenChars;
parse_line([], _OpenChars)  ->
  0;
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

% parse_line(String) ->
%   parse_line(String, 0,0,0,0).

% parse_line([], Par, Bra, Cur, Bigger) -> [Par, Bra, Cur, Bigger];
% parse_line([Head|Tail], Par, Bra, Cur, Bigger) ->
%   parse_line_char(Head, Tail, Par, Bra, Cur, Bigger).

% parse_line_char($(, Tail, Par, Bra, Cur, Bigger) ->
%   parse_line(Tail, Par+1, Bra, Cur, Bigger);
% parse_line_char($[, Tail, Par, Bra, Cur, Bigger) ->
%   parse_line(Tail, Par, Bra+1, Cur, Bigger);
% parse_line_char(${, Tail, Par, Bra, Cur, Bigger) ->
%   parse_line(Tail, Par, Bra, Cur+1, Bigger);
% parse_line_char($<, Tail, Par, Bra, Cur, Bigger) ->
%   parse_line(Tail, Par, Bra, Cur, Bigger+1);
% parse_line_char($), _Tail, Par, Bra, Cur, Bigger) when Par == 0 ->
%   parse_line([], Par-1, Bra, Cur, Bigger);
% parse_line_char($], _Tail, Par, Bra, Cur, Bigger) when Bra == 0 ->
%   parse_line([], Par, Bra-1, Cur, Bigger);
% parse_line_char($}, _Tail, Par, Bra, Cur, Bigger) when Cur == 0 ->
%   parse_line([], Par, Bra, Cur-1, Bigger);
% parse_line_char($>, _Tail, Par, Bra, Cur, Bigger) when Bigger == 0 ->
%   parse_line([], Par, Bra, Cur, Bigger-1);
% parse_line_char($), Tail, Par, Bra, Cur, Bigger) ->
%   parse_line(Tail, Par-1, Bra, Cur, Bigger);
% parse_line_char($], Tail, Par, Bra, Cur, Bigger) ->
%   parse_line(Tail, Par, Bra-1, Cur, Bigger);
% parse_line_char($}, Tail, Par, Bra, Cur, Bigger) ->
%   parse_line(Tail, Par, Bra, Cur-1, Bigger);
% parse_line_char($>, Tail, Par, Bra, Cur, Bigger) ->
%   parse_line(Tail, Par, Bra, Cur, Bigger-1).

parse_file({ok, RawData}) ->
  [unicode:characters_to_list(Line) || Line <- re:split(RawData, "[\n]+"), Line =/= <<>>];
parse_file({error, _}) ->
  "No file with that name!".
