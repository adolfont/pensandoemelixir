defmodule Mergesort do
  @moduledoc """
  Documentation for `Mergesort`.

  Source: https://gist.github.com/coproduto/1c833523680628cd25884e047e64bd7b
  """

  @doc """
  Sorts a list.


  """
  def merge_sort([]), do: []

  def merge_sort(list) do
    list
    |> Enum.map(fn x -> [x] end)
    |> merge_lists()
  end


  def merge_lists([list]), do: list

  def merge_lists(lists) do
    lists
    |> merge_pass([])
    |> merge_lists()
  end

  def merge_pass([], acc), do: acc

  def merge_pass([xs], acc), do: [xs | acc]

  def merge_pass([xs, ys | rest], acc) do
     merge_pass(rest, [merge(xs, ys) | acc])
  end

  def merge(xs, []), do: xs

  def merge([], ys), do: ys

  def merge(left = [x | xs], right = [y | ys]) do
    if y <= x do
      [y | merge(left, ys)]
    else
      [x | merge(xs, right)]
    end
  end
end
