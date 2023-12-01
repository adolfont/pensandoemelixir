defmodule Day01 do
  @digits ~w(one two three four five six seven eight nine)

  def task1(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&task1_number/1)
    |> Enum.sum()
  end

  defp task1_number(string) do
    list_with_numbers =
      string
      |> String.codepoints()
      |> Enum.filter(&filter_function/1)
      |> Enum.map(&String.to_integer/1)

    hd(list_with_numbers) * 10 + Enum.at(list_with_numbers, length(list_with_numbers) - 1)
  end

  defp filter_function(value) do
    case Integer.parse(value) do
      :error -> false
      _ -> true
    end
  end

  def task2(input) do
    input
    |> String.trim()
    |> String.split("\n")
    |> IO.inspect()
    |> Enum.map(&task2_number/1)
    |> Enum.sum()
  end

  defp task2_number(""), do: []
  defp task2_number(string) do
    parse_text_numbers =
      for number <- @digits do
        if String.starts_with?(string, number) do
          {number, String.slice(string, String.length(number), String.length(string) - 1)}
        end
      end
      |> Enum.filter(&(&1 != nil))

    IO.inspect(string)
    case parse_text_numbers do
      [{text_number, rest}] ->
        [Enum.find_index(@digits, &(&1 == text_number)) + 1 | task2_number(rest)]

      [] ->
        first = String.first(string)
        rest = String.slice(string, 1, String.length(string) - 1)

        case Integer.parse(first) do
          :error -> task2_number(rest)
          {numeral, _} -> [numeral |  task2_number(rest)]
        end
    end
    |> IO.inspect()

    10
  end
end
