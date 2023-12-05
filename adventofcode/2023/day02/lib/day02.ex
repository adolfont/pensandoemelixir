defmodule Day02 do
  def task1(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&parse_line/1)
    |> Enum.map(&possible_game?/1)
    |> Enum.filter(fn {_, y} -> y end)
    |> Enum.map(fn {x, _} -> x end)
    |> Enum.sum()
  end

  defp parse_line("Game " <> string) do
    string
    |> String.split([":", ";"], trim: true)
  end

  defp possible_game?([id | games]) do
    {String.to_integer(id), possible?(games, 12, 13, 14, true)}
  end

  defp possible?([], _, _, _, result), do: result

  defp possible?([first_game | rest], red, green, blue, result) do
    {red1, green1, blue1} = parse_game(first_game)

    if red1 > red or green1 > green or blue1 > blue do
      false
    else
      possible?(rest, red, green, blue, result)
    end
  end

  def parse_game(game_as_string) do
    keyword_list =
      game_as_string
      |> String.trim()
      |> String.split(",")
      |> Enum.map(&get_color_and_number/1)

    {Keyword.get(keyword_list, :red, 0), Keyword.get(keyword_list, :green, 0),
     Keyword.get(keyword_list, :blue, 0)}
  end

  defp get_color_and_number(string) when is_binary(string) do
    string
    |> String.trim()
    |> String.split()
    |> Enum.reverse()
    |> get_color_and_number()
  end

  defp get_color_and_number(["blue", n]) do
    {:blue, String.to_integer(n)}
  end

  defp get_color_and_number(["red", n]) do
    {:red, String.to_integer(n)}
  end

  defp get_color_and_number(["green", n]) do
    {:green, String.to_integer(n)}
  end
end
