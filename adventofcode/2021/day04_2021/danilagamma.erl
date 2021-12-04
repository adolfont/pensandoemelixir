% Source https://erlangforums.com/t/advent-of-code-2021-day-4/734/2
% 
% The part 1 of this code does not work with the input from the description. 
-module(danilagamma).

-export([main/0]).

main() ->
    % main("test_input_3.txt"),
    % main("test_input_4.txt"),
    main("input_from_description.txt"),
    main("input.txt").

main(File) ->
    {ok, RawData} = file:read_file(File),
    [NumbersRaw | CardsRaw] = bin_split(RawData, <<"\n\n">>),
    Numbers = [binary_to_integer(N) || N <- bin_split(NumbersRaw, <<",">>)],
    Cards =
        [[[binary_to_integer(N) || N <- bin_split(Rows, <<" ">>), N =/= <<>>]
          || Rows <- bin_split(C, <<"\n">>)]
         || C <- CardsRaw],
    io:format("part 1: ~p~n", [solve1(Numbers, Cards)]),
    io:format("part 2: ~p~n", [solve2(Numbers, Cards)]),
    ok.

bin_split(Binary, Subj) ->
    binary:split(Binary, Subj, [global, trim]).

solve1(Numbers, Cards) ->
    {[Winner | _], _NewCards, N, _NewNumbers} = bingo(Numbers, Cards),
    score(Winner) * N.

bingo([N | Numbers], Cards) ->
    case lists:partition(fun is_winner/1, [update_card(N, C) || C <- Cards]) of
        {[], NewCards} ->
            bingo(Numbers, NewCards);
        {Winners, NewCards} ->
            {Winners, NewCards, N, Numbers}
    end.

update_card(Number, Card) ->
    [[case X of
          Number ->
              false;
          _ ->
              X
      end
      || X <- Row]
     || Row <- Card].

is_winner(Card) ->
    check_rows(Card) orelse check_columns(Card).

check_rows(Card) ->
    lists:any(fun(Row) -> not lists:any(fun is_integer/1, Row) end, Card).

check_columns([[false | _], [false | _], [false | _], [false | _], [false | _]]) ->
    true;
check_columns([[_ | A], [_ | B], [_ | C], [_ | D], [_ | E]]) ->
    check_columns([A, B, C, D, E]);
check_columns([[], [], [], [], []]) ->
    false.

score(Card) ->
    lists:sum([N || Row <- Card, N <- Row, is_integer(N)]).

solve2(Numbers, Cards) ->
    case bingo(Numbers, Cards) of
        {[Winner], [], N, _NewNumbers} ->
            score(Winner) * N;
        {_Winners, NewCards, _N, NewNumbers} ->
            solve2(NewNumbers, NewCards)
    end.
