defmodule CorridaDeCavalos do
  @moduledoc """
  Documentation for `CorridaDeCavalos`.
  """

  @doc """
  Sorteia um número


  """
  def sorteia(n) do
    :rand.uniform(n)
  end

  def vencedor(rodadas, cavalos) do
    {vencedor, _} = corrida(rodadas, cavalos)
    vencedor
  end

  def corrida(rodadas, cavalos) do
    posicoes_iniciais =
      for i <- 1..cavalos do
        {i, 1}
      end

    corrida(rodadas, cavalos, [posicoes_iniciais])
  end

  defp corrida(1, _cavalos, [ultimo | _] = posicoes) do
    {vencedor, _posicoes} =
      Enum.max(
        ultimo,
        fn {_cavalo1, posicao1}, {_cavalo2, posicao2} -> posicao1 > posicao2 end
      )

    {vencedor, posicoes}
  end

  defp corrida(rodada, cavalos, [ultimo | _] = posicoes) do
    novas_posicoes =
      for {cavalo, posicao} <- ultimo do
        {cavalo, posicao + sorteia(2) - 1}
      end

    corrida(rodada - 1, cavalos, [novas_posicoes | posicoes])
  end

  def desenha_corrida(rodadas, cavalos) do
    {vencedor, corridas} = corrida(rodadas, cavalos)
    ordem_certa = Enum.reverse(corridas)
    IO.write(IO.ANSI.clear())
    escreve(1, 1, "Serão #{rodadas} rodadas com #{cavalos} cavalos.")

    desenha_corrida(ordem_certa)
    escreve(1, 20, "E o vencedor foi o cavalo #{vencedor}!\n")
  end

  def desenha_corrida([]), do: escreve(1, 19, "FIM!!!")

  def desenha_corrida([atual | resto]) do
    for {cavalo, posicao} <- atual do
      escreve(2+ posicao, 2 * cavalo,  "#{cavalo}")
    end

    Process.sleep(1000)
    desenha_corrida(resto)
  end

  def escreve(x, y, valor) when x > 0 and y > 0 do
    IO.write(IO.ANSI.cursor(y, x) <> valor)
  end

end

# mix run -e "CorridaDeCavalos.desenha_corrida(20, 9)"
