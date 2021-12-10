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
main("input_from_description.txt").
%    main("input.txt").

main(File) ->
Data = parse_file(file:read_file(File)),

io:format("Part 1 (, ???) for ~p: ~p~n", [File, part1(Data)]).
% io:format("Part 2 (1134, 1103130) for ~p: ~p~n", [File, part2(Data)]).

part1(Data) ->
  [parse_line(Line) || Line <- Data].

parse_line(String) -> 
  parse_line(String, [0,0},{0,0},{0,0},{0,0}).


    % ): 3 points.
    % ]: 57 points.
    % }: 1197 points.
    % >: 25137 points.


parse_line([], Par, Bra, Cur, Less) -> [Par, Bra, Cur, Less]s;
parse_line([Head|Tail], Par, Bra, Cur, Less) ->
  parse_line(Tail, update(Head, Par, Bra, Cur, Less)).

update_struct(S=[{PO, PC}$(, )



parse_file({ok, RawData}) ->
      [unicode:characters_to_list(Line) || Line <- re:split(RawData, "[\n]+"), Line =/= <<>>];
parse_file({error, _}) ->
"No file with that name!".
