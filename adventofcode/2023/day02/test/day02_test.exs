defmodule Day02Test do
  use ExUnit.Case

  test "Day 2 Task 1 description test" do
    input =
      """
      Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
      """

    assert Day02.task1(input) == 8
  end

  test "Day 2 Task 1 test" do
    input =
      File.read!("input.txt")
      |> String.trim()

    assert Day02.task1(input) == 1734
  end

  test "Day 2 Task 2 description test" do
    input =
      """
      Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
      """

    assert Day02.task2(input) == 2286
  end

  test "Day 2 Task 2 test" do
    input =
      File.read!("input.txt")
      |> String.trim()

    assert Day02.task2(input) == 000
  end
end
