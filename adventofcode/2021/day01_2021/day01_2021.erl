-module(day01_2021).

-export([depth_measurement_increases/1,
         depth_measurement_increases_from_sliding_windows/1, main/0]).

%% TASK 1
depth_measurement_increases(InputString) ->
    ListOfNumbers = convert_long_string_to_numbers(InputString),
    depth_measurement_increases(ListOfNumbers, 0).

depth_measurement_increases([First, Second], Increases) when Second > First ->
    Increases + 1;
depth_measurement_increases([_First, _Second], Increases) ->
    Increases;
depth_measurement_increases([First, Second | Tail], Increases) when Second > First ->
    depth_measurement_increases([Second | Tail], Increases + 1);
depth_measurement_increases([_First, Second | Tail], Increases) ->
    depth_measurement_increases([Second | Tail], Increases).

%% TASK 2

depth_measurement_increases_from_sliding_windows(InputString) ->
    ListOfNumbers = convert_long_string_to_numbers(InputString),
    depth_measurement_increases_from_sliding_windows(ListOfNumbers, 0, 0).

depth_measurement_increases_from_sliding_windows([Element1, Element2, Element3],
                                                 PreviousWindow,
                                                 Increases)
    when Element1 + Element2 + Element3 > PreviousWindow ->
    Increases + 1;
depth_measurement_increases_from_sliding_windows([_Element1, _Element2, _Element3],
                                                 _PreviousWindow,
                                                 Increases) ->
    Increases;
depth_measurement_increases_from_sliding_windows([E1, E2, E3, E4 | Rest],
                                                 _PreviousWindow,
                                                 Increases)
    when E4 > E1 ->
    depth_measurement_increases_from_sliding_windows([E2, E3, E4 | Rest],
                                                     E2 + E3 + E4,
                                                     Increases + 1);
depth_measurement_increases_from_sliding_windows([_E1, E2, E3, E4 | Rest],
                                                 _PreviousWindow,
                                                 Increases) ->
    depth_measurement_increases_from_sliding_windows([E2, E3, E4 | Rest],
                                                     E2 + E3 + E4,
                                                     Increases).

%% HELPER FUNCTIONS
convert_long_string_to_numbers(String) ->
    lists:map(fun(S) ->
                 {Value, _} = string:to_integer(S),
                 Value
              end,
              convert_long_string_to_list(String)).

convert_long_string_to_list(String) ->
    lists:filter(fun(Value) -> Value > "" end, string:split(String, "\n", all)).

read_input_file_into_string(InputFileName) ->
    {ok, InputFromFile} = file:read_file(InputFileName),
    unicode:characters_to_list(InputFromFile).

main() ->
    InputFromDescription = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263",
    io:format("~p~n", [depth_measurement_increases(InputFromDescription)]),
    io:format("~p~n",
              [depth_measurement_increases_from_sliding_windows(InputFromDescription)]),
    InputFromFileAsString = read_input_file_into_string("input.txt"),
    io:format("~p~n", [depth_measurement_increases(InputFromFileAsString)]),
    io:format("~p~n",
              [depth_measurement_increases_from_sliding_windows(InputFromFileAsString)]).
