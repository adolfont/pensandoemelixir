defmodule Day072022Test do
  use ExUnit.Case

  test "parse \"$ cd /\"" do
    assert Day072022.parse("$ cd /") == {:cd, :root}
  end

  test "parse \"$ ls\"" do
    assert Day072022.parse("$ ls") == {:ls}
  end

  test "parse dir" do
    assert Day072022.parse("dir a") == {:dir, "a"}
    assert Day072022.parse("dir b") == {:dir, "b"}
  end

  test "input from description" do
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

    assert Day072022.task_1(input) == 95437
  end

  # test "use my own AoC input" do
  #   assert Day072022.convert_file_to_list("input.txt") == []
  # end

  test "create a tree" do
    t = Tree.new("a")
    t = Tree.add_child(t, "b")

    c = Tree.new("c")
    c = Tree.add_child(c, "e")

    f = Tree.new("f")
    f = Tree.add_child(f, "g")
    c = Tree.add_child(c, f)
    t = Tree.add_child(t, c)

    t = Tree.add_child(t, "d")

    assert t == []
  end
end
