defmodule Day01 do
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
    |> Enum.map(&task2_number/1)
    |> Enum.sum()
  end

  defp task2_number(str) do
    list_with_numbers = task2_number(str, [])

    hd(list_with_numbers) * 10 + Enum.at(list_with_numbers, length(list_with_numbers) - 1)
  end

  defp task2_number("", current), do: Enum.reverse(current)

  defp task2_number("one" <> rest, current) do
    task2_number("ne" <> rest, [1 | current])
  end

  defp task2_number("two" <> rest, current) do
    task2_number("wo" <> rest, [2 | current])
  end

  defp task2_number("three" <> rest, current) do
    task2_number("hree" <> rest, [3 | current])
  end

  defp task2_number("four" <> rest, current) do
    task2_number("our" <> rest, [4 | current])
  end

  defp task2_number("five" <> rest, current) do
    task2_number("ive" <> rest, [5 | current])
  end

  defp task2_number("six" <> rest, current) do
    task2_number("ix" <> rest, [6 | current])
  end

  defp task2_number("seven" <> rest, current) do
    task2_number("even" <> rest, [7 | current])
  end

  defp task2_number("eight" <> rest, current) do
    task2_number("ight" <> rest, [8 | current])
  end

  defp task2_number("nine" <> rest, current) do
    task2_number("ine" <> rest, [9 | current])
  end

  defp task2_number(str, current) do
    possible_value = String.first(str)
    rest = String.slice(str, 1, String.length(str) - 1)

    case Integer.parse(possible_value) do
      :error -> task2_number(rest, current)
      {numeral, _} -> task2_number(rest, [numeral | current])
    end
  end
end
