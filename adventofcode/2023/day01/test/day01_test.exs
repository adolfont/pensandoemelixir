defmodule Day01Test do
  use ExUnit.Case

  @input File.read!("input.txt") |> String.trim()

  test "task1 description test" do
    input =
      """
      1abc2
      pqr3stu8vwx
      a1b2c3d4e5f
      treb7uchet
      """

    assert Day01.task1(input) == 142
  end

  test "task2 description test" do
    input =
      """
      two1nine
      eightwothree
      abcone2threexyz
      xtwone3four
      4nineeightseven2
      zoneight234
      7pqrstsixteen
      """

    assert Day01.task2(input) == 281
  end

  test "task2 - another test" do
    input =
      """
      seveneighteightfour1
      58twoxgklhpndxjrpb86
      five2sixfourcjfvnmhrxrtwovhrdrfrssphgtcqthhzxh
      lxtbmsevenbms3one8dsbsixnine
      sevenhcgr6ninefour
      trknlxnv43zxlrqjtwonect
      """

    assert Day01.task2(input) == 71 + 56 + 52 + 79 + 74 + 41
  end

  test "task2 - yet another test" do
    input =
      """
      7onetjjkznvlb
      93two4foureight
      8fqddclzvlx
      tdpcspmg39ddqkdlpjxvkdtjpc21
      fivessmncpxsd3eighttwone
      """

    assert Day01.task2(input) == 71 + 98 + 88 + 31 + 51
  end

  test "task1 - my input" do
    assert Day01.task1(@input) == 56049
  end

  test "task2 - my input" do
    assert Day01.task2(@input) == 54530
  end
end
