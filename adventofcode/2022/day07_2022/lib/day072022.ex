defmodule Day072022 do
  # INCOMPLETE

  def parse("$ cd /"), do: {:cd, :root}
  def parse("$ cd .."), do: {:cd, :up}
  def parse("$ cd " <> folder), do: {:cd, folder}
  def parse("$ ls"), do: {:ls}
  def parse("dir " <> folder), do: {:dir, folder}

  def parse(file) do
    [size, name | []] = String.split(file)
    {:file, String.to_integer(size), name}
  end

  def task_1(input) do
    commands =
      input
      |> Day072022.convert_string_to_list()
      |> Enum.map(&Day072022.parse/1)

    process(commands)
  end

  defp process([{:cd, :root} | commands]) do
    process(commands, Tree.new("/"))
  end

  defp process([{:ls} | commands], tree) do
    process_ls(commands, tree)
  end

  def process_ls([{:file, size, name} | commands], tree) do
    tree = Tree.add_child(tree, {:file, size, name})
    process_ls(commands, tree)
  end

  def process_ls([{:dir, dirname} | commands], tree) do
    tree = Tree.add_child(tree, {:dir, dirname})
    process_ls(commands, tree)
  end

  def process_ls([{:cd, _} | _] = commands, tree) do
    process(commands, tree)
  end

  def convert_file_to_list(filename) do
    {:ok, string} = File.read(filename)

    string
    |> String.trim()
    |> String.split("\n")
  end

  def convert_string_to_list(string) do
    string
    |> String.trim()
    |> String.split("\n")
  end
end
