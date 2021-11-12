defmodule LanguageList do
  def adiciona(lista, linguagem) do
    [linguagem | lista]
  end

  def esta_na?(linguagem, lista) do
    linguagem in lista
  end
end
