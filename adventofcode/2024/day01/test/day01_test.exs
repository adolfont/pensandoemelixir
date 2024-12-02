defmodule Day01Test do
  use ExUnit.Case
  doctest Day01

  test "Task 1 description test" do
    test_input =
      """
      3   4
      4   3
      2   5
      1   3
      3   9
      3   3
      """

    assert Day01.task1(test_input) == 11
  end

  test "Task 1 second test" do
    test_input =
      """
      3   4
      4   3
      2   5
      """

    assert Day01.task1(test_input) == 3
  end

  test "Small sample" do
    test_input =
      """
      100   200
      200   300
      300   400
      400   500
      """

    assert Day01.task1(test_input) == 400
  end

  test "Task 1 with my input" do
    {:ok, test_input} = File.read("test/myinput.txt")
    assert Day01.task1(test_input) == 3_508_942
  end

  test "Similarity score" do
    assert Day01.similarity_score(3, [1, 2, 3, 5, 4, 3]) == 3 * 2
    assert Day01.similarity_score(4, [1, 2, 3, 5, 4, 3]) == 4 * 1
    assert Day01.similarity_score(6, [1, 2, 3, 5, 4, 3]) == 6 * 0
    assert Day01.similarity_score(2, [2, 2, 2, 5, 2, 3]) == 2 * 4
  end

  test "Task 2 description test" do
    test_input =
      """
      3   4
      4   3
      2   5
      1   3
      3   9
      3   3
      """

    assert Day01.task2(test_input) == 31
  end

  test "Task 2 with my input" do
    {:ok, test_input} = File.read("test/myinput.txt")
    assert Day01.task2(test_input) == 26_593_248
  end
end
