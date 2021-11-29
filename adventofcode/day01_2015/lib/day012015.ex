defmodule Day012015 do
  @moduledoc """
  Documentation for `Day012015`.

  https://adventofcode.com/2015/day/1

  """

  @doc """
  Find the right floor.

  ## Examples

      iex> Day012015.find_floor("(())")
      0
      iex> Day012015.find_floor("()()")
      0
      iex> Day012015.find_floor("(((")
      3
      iex> Day012015.find_floor("(()(()(")
      3
      iex> Day012015.find_floor("))(((((")
      3
      iex> Day012015.find_floor("())")
      -1
      iex> Day012015.find_floor("))(")
      -1
      iex> Day012015.find_floor(")))")
      -3
      iex> Day012015.find_floor(")())())")
      -3
      iex>  Day012015.find_floor(Day012015.convert_file_to_string("lib/my_input.txt"))
      138
      iex> Day012015.find_position("())))")
      3
      iex> Day012015.find_position(Day012015.convert_file_to_string("lib/my_input.txt"))
      1771

  """
  def convert_file_to_string(filename) do
    # File.read(filename) |>
    # Kernel.then(fn {:ok, string} -> String.trim(string) end)
    {:ok, string} = File.read(filename)
    string
  end

  def find_floor(string) do
    find_floor(string, 0)
  end

  def find_floor("", current), do: current

  def find_floor("(" <> rest, current) do
    find_floor(rest, current + 1)
  end

  def find_floor(")" <> rest, current) do
    find_floor(rest, current - 1)
  end

  def find_position(string) do
    find_position(string, 0, 0)
  end

  def find_position(_, position, -1), do: position

  def find_position("(" <> rest, position, floor) do
    find_position(rest, position + 1, floor + 1)
  end

  def find_position(")" <> rest, position, floor) do
    find_position(rest, position + 1, floor - 1)
  end
end
