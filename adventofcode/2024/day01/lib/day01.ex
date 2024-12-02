defmodule Day01 do
  # https://adventofcode.com/2024/day/1
  # https://chatgpt.com/share/674db269-a638-8002-bd7b-36509db993a0

  def task1(input) do
    {list1, list2} = get_lists(input)
    calculate(Enum.sort(list1), Enum.sort(list2))
  end

  def get_lists(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, "   "))
    |> Enum.map(fn [a, b] -> {String.to_integer(a), String.to_integer(b)} end)
    |> Enum.unzip()
  end

  def calculate(list1, list2) do
    Enum.zip(list1, list2)
    |> Enum.reduce(0, fn {a, b}, acc -> acc + abs(b - a) end)
  end

  def similarity_score(n, list) do
    count = Enum.count(list, fn x -> x == n end)
    n * count
  end

  def task2(input) do
    {list1, list2} = get_lists(input)

    list1
    |> Enum.map(fn n -> similarity_score(n, list2) end)
    |> Enum.sum()
  end
end
