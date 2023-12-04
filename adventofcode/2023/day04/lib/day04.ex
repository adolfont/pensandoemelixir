defmodule Day04 do
  def part1(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> IO.inspect()
    |> Enum.map(&get_points/1)
    |> IO.inspect()
    |> Enum.sum()
  end

  defp get_points(input) do
    list =
      input
      |> String.split()
      |> Enum.drop(2)

    bar_position = Enum.find_index(list, fn x -> x == "|" end)
    winning_numbers = MapSet.new(Enum.slice(list, 0..(bar_position - 1)))
    numbers = MapSet.new(Enum.slice(list, (bar_position + 1)..(length(list) - 1)))

    size =
      MapSet.intersection(winning_numbers, numbers)
      |> MapSet.size()

    if size > 0 do
      2 ** (size - 1)
    else
      0
    end
  end
end
