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
      iex>  File.read("lib/my_input.txt") |> Kernel.then(fn {:ok, string} -> String.trim(string) end) |> Day012015.find_floor()
      138

  """
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
end
