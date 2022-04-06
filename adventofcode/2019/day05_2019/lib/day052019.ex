defmodule Day052019 do
  def convert_file_to_string(filename) do
    {:ok, string} = File.read(filename)
    string
    |> String.trim()
    |> String.split(",")
  end

end
