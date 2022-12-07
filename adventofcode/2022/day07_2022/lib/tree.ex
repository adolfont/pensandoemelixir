defmodule Tree do
  def new(node) do
    Map.new("")
  end

  def add_child(tree, node) do
    tree ++ [node]
  end
end
