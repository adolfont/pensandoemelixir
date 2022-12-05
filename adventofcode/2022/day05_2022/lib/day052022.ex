defmodule Day052022 do
  # @input """
  # [D]
  # [N] [C]
  # [Z] [M] [P]
  #  1   2   3

  # move 1 from 2 to 1
  # move 3 from 1 to 3
  # move 2 from 2 to 1
  # move 1 from 1 to 2
  # """

  @input """
  [X]     [A]
  [D]     [A]
  [N] [C] [B]
  [Z] [M] [P]
   1   2   3

  move 1 from 2 to 1
  move 3 from 1 to 3
  move 2 from 2 to 1
  move 1 from 1 to 2
  """

  def process_input(input) do
    [stacks, instructions] = String.split(input, "\n\n")
    {process_stacks(stacks), process_instructions(instructions)}
  end

  def process_stacks(stacks) do
    stacks =
      stacks
      |> String.split("\n")
      |> Enum.reverse()

    number_of_stacks =
      stacks
      |> hd()
      |> String.trim()
      |> String.split()
      |> Enum.reverse()
      |> hd()
      |> String.to_integer()

    {number_of_stacks, process_stack_data(stacks, number_of_stacks)}
  end

  defp process_stack_data([_ | tail], _number_of_stacks) do
    tail
    |> Enum.reverse()
    |> Enum.map(&process_line/1)
    |> create_map()
  end

  defp create_map(list) do
    create_map(list, length(list), Map.new())
  end

  defp create_map([], _length, map) do
    map
  end

  defp create_map([head | tail], length, map) do
    create_map(tail, length - 1, add_line(map, length, head))
  end

  defp add_line(map, current_column, line) do
    line
    |> Enum.with_index()
    |> Enum.reduce(
      map,
      fn {value, line_index}, acc ->
        if value != " " do
          Map.put(acc, {line_index + 1, current_column}, value)
        else
          acc
        end
      end
    )
  end

  defp process_line(line) do
    line
    |> String.codepoints()
    |> Enum.chunk_every(4)
    |> Enum.map(&Enum.at(&1, 1))
  end

  def process_instructions(instructions) do
    instructions
    |> String.split("\n", trim: true)
    |> Enum.map(fn str ->
      str
      |> String.split()
      |> Enum.drop_every(2)
    end)
  end

  def main() do
    @input
    |> process_input()
    |> IO.inspect()

    :ok
  end
end

Day052022.main()
