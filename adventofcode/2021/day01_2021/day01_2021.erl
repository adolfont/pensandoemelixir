-module(day01_2021).

-export([depth_measurement_increases/1, main/0]).

depth_measurement_increases(InputString) ->
    ListOfNumbers = convert_long_string_to_numbers(InputString),
    depth_measurement_increases(ListOfNumbers, 0).

depth_measurement_increases([First, Second], Increases) when Second > First ->
    Increases + 1;
depth_measurement_increases([_First, _Second], Increases) ->
    Increases;
depth_measurement_increases([First, Second | Tail], Increases) 
    when Second > First ->
    depth_measurement_increases([Second | Tail], Increases + 1);
depth_measurement_increases([_First, Second | Tail], Increases) ->
    depth_measurement_increases([Second | Tail], Increases).

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
    InputFromFileAsString = read_input_file_into_string("input.txt"),
    io:format("~p~n", [depth_measurement_increases(InputFromFileAsString)]).
