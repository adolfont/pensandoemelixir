defmodule Day102022 do
  def read(filename) do
    {:ok, string} = File.read(filename)

    string
  end

  def task1(string) do
    string
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&parse/1)
    |> List.flatten()
    |> Enum.reduce({1, 1, 0}, &f/2)
    |> Tuple.to_list()
    |> Enum.at(2)
  end

  defp parse("noop") do
    [0]
  end

  defp parse("addx " <> value) do
    [0, String.to_integer(value)]
  end

  defp f(value, {x, line, sum}) do
    if(rem(line - 20, 40) == 0) do
      sum = sum + line * x

      {x + value, line + 1, sum}
    else
      {x + value, line + 1, sum}
    end
  end
end
