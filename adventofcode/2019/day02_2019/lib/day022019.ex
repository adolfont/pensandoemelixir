defmodule Day022019 do
  def convert_file_to_map(filename) do
    {:ok, string} = File.read(filename)

    list =
      string
      |> String.trim()
      |> String.split(",")
      |> Enum.map(&String.to_integer/1)

    0..(length(list) - 1) |> Stream.zip(list) |> Enum.into(%{})
  end

  def process_instruction(map, position) do
    # IO.puts("Instrução: #{map[position]}")

    cond do
      map[position] == 99 ->
        map

      map[position] == 1 ->
        add(map, position)
        |> process_instruction(position + 4)

      map[position] == 2 ->
        multiply(map, position)
        |> process_instruction(position + 4)
    end
  end

  def add(map, position) do
    position_input_1 = map[position + 1]
    position_input_2 = map[position + 2]
    position_output = map[position + 3]
    %{map | position_output => map[position_input_1] + map[position_input_2]}
  end

  def multiply(map, position) do
    position_input_1 = map[position + 1]
    position_input_2 = map[position + 2]
    position_output = map[position + 3]
    %{map | position_output => map[position_input_1] * map[position_input_2]}
  end

  def process_file(filename) do
    filename
    |> convert_file_to_map()
    |> process_instruction(0)
    |> Map.get(0)
  end

  def process_file_and_change(filename) do
    filename
    |> convert_file_to_map()
    |> Map.put(1, 12)
    |> Map.put(2, 2)
    |> process_instruction(0)
    |> Map.get(0)
  end
end
