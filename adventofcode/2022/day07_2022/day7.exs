defmodule Day7 do
  @moduledoc """
  https://adventofcode.com/2022/day/7
  """

#  Adapted from https://github.com/ed-flanagan/advent-of-code-solutions-elixir/blob/main/lib/advent/y2022/d07.ex
#  Found at https://elixirforum.com/t/advent-of-code-2022-day-7/52324/5

  @doc """
  Find all of the directories with a total size of at most 100000. What is the
  sum of the total sizes of those directories?
  """
  @spec part_one(Enumerable.t()) :: non_neg_integer()
  def part_one(input) do
    input
    # My initial impl had parsing/filter/sum bundled into one function. However,
    # I split out parsing to make more generic for part 2
    |> parse_input()
    |>  IO.inspect(label: "the list")
    |> Enum.filter(&(&1 <= 100_000))
    |>  IO.inspect(label: "the filtered list")
    |> Enum.sum()
  end

  @doc """
  Find the smallest directory that, if deleted, would free up enough space on
  the filesystem to run the update. What is the total size of that directory?
  """
  @spec part_two(Enumerable.t()) :: non_neg_integer()
  def part_two(input) do
    [root | children] = parse_input(input)
    to_free = root - 40_000_000

    children
    |> Enum.filter(&(&1 >= to_free))
    |> Enum.min()
  end

  defp parse_input(input) do
    parse_input(input, [], [])
  end

  defp parse_input(["$ cd .." | tail], [cwd, parent | parents], acc) do
    parse_input(tail, [cwd + parent | parents], [cwd | acc])
  end

  defp parse_input(["$ cd " <> _ | tail], parents, acc) do
    parse_input(tail, [0 | parents], acc)
  end

  # Skip these two
  defp parse_input(["$ ls" | tail], parents, acc), do: parse_input(tail, parents, acc)
  defp parse_input(["dir " <> _ | tail], parents, acc), do: parse_input(tail, parents, acc)

  defp parse_input([file | tail], [cwd | parents], acc) do
    [fsize, _] = String.split(file)
    parse_input(tail, [cwd + String.to_integer(fsize) | parents], acc)
  end

  defp parse_input([], [], acc), do: acc
  defp parse_input([], [root], acc), do: [root | acc]

  defp parse_input([], [cwd, parent | parents], acc) do
    parse_input([], [cwd + parent | parents], [cwd | acc])
  end

  def convert_string_to_list(string) do
    string
    |> String.trim()
    |> String.split("\n")
  end

  def convert_file_to_list(filename) do
    {:ok, string} = File.read(filename)

    string
    |> String.trim()
    |> String.split("\n")
  end
end

input = """
$ cd /
$ ls
dir b
dir d
$ cd b
10 some
11 some2
dir c
$ cd c
13 some3
$ cd ..
$ cd ..
$ cd d
$ ls
14 some4
15 some5
"""


IO.inspect(Day7.part_one(Day7.convert_string_to_list(input)))

input = """
$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
"""

IO.inspect(Day7.part_one(Day7.convert_string_to_list(input)))




filename = "input.txt"

Day7.convert_file_to_list(filename)
|> Day7.part_one()
|> IO.inspect()

Day7.convert_file_to_list(filename)
|> Day7.part_two()
|> IO.inspect()
