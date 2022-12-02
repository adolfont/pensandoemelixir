input = """
A Y
B X
C Z
"""

defmodule Day2 do
  @points_part1 %{
    "A X" => 1 + 3,
    "A Y" => 2 + 6,
    "A Z" => 3 + 0,
    "B X" => 1 + 0,
    "B Y" => 2 + 3,
    "B Z" => 3 + 6,
    "C X" => 1 + 6,
    "C Y" => 2 + 0,
    "C Z" => 3 + 3
  }

  # A, X ROCK
  # B, Y PAPER
  # C, Z SCISSORS
  #
  # X lose,
  # Y  draw,
  # Z  win
  #
  # 0 if you lost, 3 if the round was a draw, and 6 if you won

  @points_part2 %{
    # Z
    "A X" => 3 + 0,
    # X
    "A Y" => 1 + 3,
    # Y
    "A Z" => 2 + 6,
    # X
    "B X" => 1 + 0,
    # Y
    "B Y" => 2 + 3,
    # Z
    "B Z" => 3 + 6,
    # Y
    "C X" => 2 + 0,
    # Z
    "C Y" => 3 + 3,
    # X
    "C Z" => 1 + 6
  }

  def part1(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.map(fn x -> @points_part1[x] end)
    |> Enum.sum()
    |> IO.inspect()
  end

  def part2(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&String.trim/1)
    |> Enum.map(fn x -> @points_part2[x] end)
    |> Enum.sum()
    |> IO.inspect()
  end
end

Day2.part1(input)
Day2.part2(input)
