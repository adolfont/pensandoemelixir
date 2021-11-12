defmodule LanguageListTest do
  use ExUnit.Case

  import LanguageList

  test "Adiciona Elixir à lista de linguagens" do
    assert LanguageList.adiciona([], "Elixir") == ["Elixir"]
  end

  test "Adiciona C e Elixir à lista de linguagens" do
    lista =
      adiciona([], "C")
      |> adiciona("Elixir")

    assert lista == ["Elixir", "C"]
  end

  test "Adiciona C, Java e Elixir à lista de linguagens" do
    lista =
      adiciona([], "C")
      |> adiciona("Java")
      |> adiciona("Elixir")

    assert lista == ["Elixir", "Java", "C"]
  end

  test "Elixir está na lista de linguagens contendo C, Java e Elixir" do
    lista =
      adiciona([], "C")
      |> adiciona("Java")
      |> adiciona("Elixir")

    assert LanguageList.esta_na?("Elixir", lista)
  end
end
