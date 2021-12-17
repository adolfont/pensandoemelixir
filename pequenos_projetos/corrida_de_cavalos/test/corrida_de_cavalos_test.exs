defmodule CorridaDeCavalosTest do
  use ExUnit.Case
  doctest CorridaDeCavalos

  test "geração de números pseudoaleatórios" do
    :rand.seed(:exrop, {1000, 2000, 3000})
    assert CorridaDeCavalos.sorteia(6) == 6
    assert CorridaDeCavalos.sorteia(6) == 5
    assert CorridaDeCavalos.sorteia(6) == 1
    assert CorridaDeCavalos.sorteia(6) == 1
    assert CorridaDeCavalos.sorteia(6) == 2
  end

  test "Corrida de 10 rodadas com 6 cavalos" do
    :rand.seed(:exrop, {1000, 2000, 3000})
    assert CorridaDeCavalos.vencedor(10, 6) == 5
    assert CorridaDeCavalos.vencedor(10, 6) == 3

    mais_corridas =
      for _ <- 1..30 do
        CorridaDeCavalos.vencedor(10, 6)
      end

    assert mais_corridas == [
             1,
             6,
             1,
             5,
             4,
             3,
             5,
             1,
             6,
             6,
             4,
             3,
             5,
             5,
             6,
             6,
             3,
             3,
             5,
             6,
             1,
             6,
             6,
             5,
             5,
             3,
             3,
             2,
             3,
             4
           ]
  end
end
