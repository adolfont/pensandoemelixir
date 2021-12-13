-module(day13).

-include_lib("eunit/include/eunit.hrl").

% Problem: https://adventofcode.com/2021/day/13
%
% erlc -DTEST day13.erl && erl -noshell -pa . -eval "eunit:test(day13, [verbose])" -s init stop
% OR
% erlc day13.erl && erl -noshell -pa . -eval "day13:main()" -s init stop
%

% learned: nothing new today

-export([main/0]).

-ifdef(EUNIT).

-endif.

main() ->
  main("input_from_description.txt"),
  % main("small_input.txt"),
  main("input.txt").

main(File) ->
  Data = parse_file(file:read_file(File)),

  io:format("Part 1 (17, 790) for ~p: ~n~p~n", [File, part1(Data)]),
  io:format("Part 2 (O, PGHZBFJC) for ~p: ~n~p~n", [File, part2(Data)]).

part1(Data) ->
  {Points, FoldingInstructions} = Data,
  % io:format("Points sorted by Y~n~p~n", [lists:sort(fun sorter/2, Points)]),
  % io:format("Points sorted by X~n~p~n", [lists:sort(Points)]),
  length(apply_fold(hd(FoldingInstructions), Points)).

% sorter({X1, Y1}, {X2, Y2}) ->
%   (Y1 < Y2) or (Y1 =:= Y2) and (X1 < X2).

part2(Data) ->
  {Points, FoldingInstructions} = Data,
  draw(part2(FoldingInstructions, Points)).

draw(Points) ->
  M = maps:from_list(Points),
  MaxX =
    lists:max(
      maps:keys(M)),
  MaxY =
    lists:max(
      maps:values(M)),
  {MaxX, MaxY},
  NewMap = maps:from_list([{P, 1} || P <- Points]),
  [[char(X, Y, NewMap) || X <- lists:seq(0, MaxX)] || Y <- lists:seq(0, MaxY)].

char(X, Y, Map) ->
  case maps:get({X, Y}, Map, 0) of
    1 ->
      $x;
    0 ->
      $.
  end.

part2([], Points) ->
  Points;
part2([Head | Tail], Points) ->
  part2(Tail, apply_fold(Head, Points)).

apply_fold({"y", Num}, Points) ->
  NewPoints = [{PX, 2 * Num - PY} || {PX, PY} <- Points, PY > Num],
  create_new_board(NewPoints, Points, Num);
apply_fold({"x", Num}, Points) ->
  NewPoints = [{2 * Num - PX, PY} || {PX, PY} <- Points, PX > Num],
  create_new_board_X(NewPoints, Points, Num).

create_new_board(NewPoints, Points, Num) ->
  % lists:usort(NewPoints
  %             ++ [{PX, max(PY, value({PX, PY}, NewPoints))} || {PX, PY} <- Points, PY < Num]).
  lists:usort(NewPoints ++ [{PX, PY} || {PX, PY} <- Points, PY < Num]).

create_new_board_X(NewPoints, Points, Num) ->
  % lists:usort(NewPoints
  %                   ++ [{max(PX, value({PX, PY}, NewPoints)), PY}
  %                   || {PX, PY} <- Points, PX < Num]).
  lists:usort(NewPoints ++ [{PX, PY} || {PX, PY} <- Points, PX < Num]).

% value({X, Y}, Points) ->
%   value(lists:member({X, Y}, Points)).

% value(true) ->
%   1;
% value(false) ->
%   0.

  % lists:foldl(fun(Point, Accu) -> Accu#{Point => 1} end, #{}, Points).

parse_file({ok, RawData}) ->
  {Points, FoldingInstructions} =
    lists:partition(fun(X) -> string:slice(X, 0, 2) =/= "fo" end,
                    [unicode:characters_to_list(Line)
                     || Line <- re:split(RawData, "[\n]+"), Line =/= <<>>]),
  ParsedPoints = [get_point(P) || P <- Points],
  ParsedInstructions = [get_instruction(I) || I <- FoldingInstructions],
  {ParsedPoints, ParsedInstructions};
parse_file({error, _}) ->
  "No file with that name!".

get_point(P) ->
  [X, Y] = re:split(P, ",+"),
  {binary_to_integer(X), binary_to_integer(Y)}.

get_instruction(I) ->
  [X, Y] = re:split(I, "=+"),
  {last_char(X), binary_to_integer(Y)}.

last_char(Bin) ->
  S = unicode:characters_to_list(Bin),

  string:slice(S, length(S) - 1, 1).
