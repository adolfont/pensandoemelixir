defmodule EncontreOTelefone do
  @associacao %{
    "ABC" => "2",
    "DEF" => "3",
    "GHI" => "4",
    "JKL" => "5",
    "MNO" => "6",
    "PQRS" => "7",
    "TUV" => "8",
    "WXYZ" => "9"
  }

  def converte_letra(letra) when letra >= "A" and letra <= "Z" do
    @associacao
    |> Map.keys()
    |> Enum.find(fn x -> String.contains?(x, letra) end)
    |> then(fn chave -> @associacao[chave] end)
  end

  def converte_letra(caracter), do: caracter

  def converte(string) do
    string
    |> String.codepoints()
    |> Enum.map(&converte_letra/1)
    |> Enum.join()
  end
end
