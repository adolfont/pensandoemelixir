defmodule Day05 do
  def task1(input) do
    lines =
      input
      |> String.trim()
      |> String.split("\n")
      |> Enum.reject(fn x -> x == "" end)

    get_seeds(lines)
  end

  defp get_seeds(lines) do
    seeds =
      lines
      |> hd()
      |> String.trim_leading("seeds: ")
      |> String.split(" ")
      |> Enum.map(fn x -> String.to_integer(x) end)

    keywords = [
      "seed-to-soil map:",
      "soil-to-fertilizer map:",
      "fertilizer-to-water map:",
      "water-to-light map:",
      "light-to-temperature map:",
      "temperature-to-humidity map:",
      "humidity-to-location map:"
    ]

    rest =
      Enum.chunk_by(tl(lines), fn x -> x in keywords end)

    only_numbers =
      rest
      |> Enum.filter(fn x -> length(x) != 1 end)

    {seeds,
     only_numbers
     |> Enum.map(fn list ->
       list
       |> Enum.map(&String.split/1)
       |> Enum.map(fn x -> Enum.map(x, &String.to_integer/1) end)
     end)}
  end
end
