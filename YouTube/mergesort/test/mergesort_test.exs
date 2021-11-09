defmodule MergesortTest do
  use ExUnit.Case
  doctest Mergesort

  @list_size 1000

  test "Mergesort sorts a shuffled list" do
    shuffled_list =
      1..@list_size
      |> Enum.shuffle()

    assert Mergesort.merge_sort(shuffled_list) == Enum.sort(shuffled_list)
  end

  test "Mergesort sorts an ordered list" do
    ordered_list =
      1..@list_size
      |> Enum.shuffle()
      |> Enum.sort()

    assert Mergesort.merge_sort(ordered_list) == ordered_list
  end

  test "Mergesort sorts a reverse ordered list" do
    reverse_ordered_list =
      1..@list_size
      |> Enum.shuffle()
      |> Enum.sort()
      |> Enum.reverse()

    assert Mergesort.merge_sort(reverse_ordered_list) == Enum.sort(reverse_ordered_list)
  end

  test "Tests for auxiliary function merge_lists/1" do
    list_with_one_element_which_is_a_list = [[1]]

    assert Mergesort.merge_lists(list_with_one_element_which_is_a_list) == [1]

    list_for_this_test = [
      [1],
      [5],
      [2],
      [4]
    ]

    assert Mergesort.merge_lists(list_for_this_test) == [1, 2, 4, 5]

    list_for_this_test = [[1, 2, 3, 5], [2, 3, 4, 8]]

    assert Mergesort.merge_lists(list_for_this_test) == [1, 2, 2, 3, 3, 4, 5, 8]

    list_for_this_test = [[1, 2, 5], [2]]

    assert Mergesort.merge_lists(list_for_this_test) == [1, 2, 2, 5]

    list_for_this_test = [[1, 2, 5], [2], [0]]

    assert Mergesort.merge_lists(list_for_this_test) == [0, 1, 2, 2, 5]
  end

  test "Tests for auxiliary function merge_pass/2" do
    list_with_one_element_which_is_a_list = [[1]]

    assert Mergesort.merge_pass(list_with_one_element_which_is_a_list, []) == [[1]]

    list_for_this_test = [
      [2],
      [1],
      [5],
      [4]
    ]

    assert Mergesort.merge_pass(list_for_this_test, []) ==
             [[4, 5], [1, 2]]

    list_for_this_test = [
      [2],
      [1],
      [5],
      [4],
      [0]
    ]

    assert Mergesort.merge_pass(list_for_this_test, []) ==
             [[0], [4, 5], [1, 2]]

    list_for_this_test = [[1], [5], [2], [3], [8], [2], [4], [3]]

    assert Mergesort.merge_pass(
             list_for_this_test,
             []
           )
           |> Mergesort.merge_pass([]) ==
             [[1, 2, 3, 5], [2, 3, 4, 8]]
  end

  test "Tests for auxiliary function merge/2" do
    assert Mergesort.merge([1, 3], [2, 4]) == [1, 2, 3, 4]

    first_list =
      1..@list_size
      |> Enum.shuffle()
      |> Enum.take(div(@list_size, 2))
      |> Enum.sort()

    second_list =
      1..@list_size
      |> Enum.shuffle()
      |> Enum.take(div(@list_size, 2))
      |> Enum.sort()

    assert Mergesort.merge(first_list, second_list) == Enum.sort(first_list ++ second_list)
  end
end
