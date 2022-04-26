defmodule EncontreOTelefoneTest do
  use ExUnit.Case

  test "Se a entrada for A o número é 2" do
    assert EncontreOTelefone.converte_letra("A") == "2"
  end

  test "Se a entrada for B o número é 2" do
    assert EncontreOTelefone.converte_letra("B") == "2"
  end

  test "Se a entrada for D o número é 3" do
    assert EncontreOTelefone.converte_letra("D") == "3"
  end

  test "Se a entrada for J o número é 5" do
    assert EncontreOTelefone.converte_letra("J") == "5"
  end

  test "Se a entrada for O o número é 6" do
    assert EncontreOTelefone.converte_letra("O") == "6"
  end

  test "Se a entrada for S o número é 7" do
    assert EncontreOTelefone.converte_letra("S") == "7"
  end

  test "Se a entrada for Y o número é 9" do
    assert EncontreOTelefone.converte_letra("Y") == "9"
  end

  test "Se a entrada for Z o número é 7" do
    assert EncontreOTelefone.converte_letra("Z") == "9"
  end

  test "Se a entrada for 3 o número é 3" do
    assert EncontreOTelefone.converte_letra("3") == "3"
  end

  test "Se a entrada for - o número é -" do
    assert EncontreOTelefone.converte_letra("-") == "-"
  end

  test "Se a entrada for 1-HOME-SWEET-HOME a saída será 1-4663-79338-4663" do
    assert EncontreOTelefone.converte("1-HOME-SWEET-HOME") == "1-4663-79338-4663"
  end

  test "Se a entrada for MY-MISERABLE-JOB a saída será 69-647372253-562" do
    assert EncontreOTelefone.converte("MY-MISERABLE-JOB") == "69-647372253-562"
  end

  test "Se a entrada for MY-LOVE a saída será 69-5683" do
    assert EncontreOTelefone.converte("MY-LOVE") == "69-5683"
  end
end
