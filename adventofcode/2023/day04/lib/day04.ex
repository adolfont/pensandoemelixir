defmodule Day04 do
  def part1(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&get_points/1)
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

  def part2(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&get_points2/1)
    |> process_result()

    0
  end

  defp get_points2(input) do
    list =
      input
      |> String.split()
      |> Enum.drop(2)

    bar_position = Enum.find_index(list, fn x -> x == "|" end)
    winning_numbers = MapSet.new(Enum.slice(list, 0..(bar_position - 1)))
    numbers = MapSet.new(Enum.slice(list, (bar_position + 1)..(length(list) - 1)))

    MapSet.intersection(winning_numbers, numbers)
    |> MapSet.size()
  end

  defp process_result(list) do

    all_ones = List.duplicate(1, length(list))

    zipped = Enum.zip(
      list,
      all_ones
    )

    process_result_2(zipped, all_ones,1 )
    |> IO.inspect()

    0
    # result = process_result(list, 0, List.duplicate(1, length(list)))
  end

  defp process_result_2([], result, _), do: result
  defp process_result_2([head | tail] = list, result, n) do
      IO.inspect({head, list, result})

      {num, _} = head

      process_result_2(tail, increment(result, n, num), n+1)
  end


  # defp process_result([], _, counter), do: counter

  # defp process_result([head | tail]=list, position, counter) do
  #   process_result(tail, position + 1, increment(counter, position+1, head))
  # end

  def increment(list, from_position, amount_of_values) do
    IO.inspect {list, from_position, amount_of_values}
    for i <- 0..length(list)-1 do
      if (i >= from_position) and (i < from_position  +  amount_of_values) do
        Enum.at(list, i)  + 1
      else
        Enum.at(list, i)
      end
    end
    |> IO.inspect()

  end
end
