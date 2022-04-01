defmodule Day022019Test do
  use ExUnit.Case

  test "Tests input from description" do
    assert Day022019.process_file("input_from_description.txt") ==
             3500
  end

  test "Tests my input" do
    assert Day022019.process_file_and_change("input.txt") ==
             5_290_681
  end
end
