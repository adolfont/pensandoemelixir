defmodule Day102022v2 do
  use GenServer

  ## GENSERVER CALLBACKS

  @impl true
  def init(counter) do
    {:ok, counter}
  end

  @impl true
  def handle_call({:filename, filename}, _from, counter) do
    {:reply, task1_file(filename), counter + 1}
  end

  @impl true
  def handle_call({:string, string}, _from, counter) do
    {:reply, task1(string), counter + 1}
  end

  @impl true
  def handle_call({:part2_string, string}, _from, counter) do
    {:reply, task2(string), counter + 1}
  end

  @impl true
  def handle_call({:part2_filename, filename}, _from, counter) do
    {:reply, task2_file(filename), counter + 1}
  end

  ## Client API

  @doc """
  """
  def start_link(counter) do
    GenServer.start_link(__MODULE__, counter)
  end

  def process(server, {:string, string}) do
    GenServer.call(server, {:string, string})
  end

  def process(server, {:filename, filename}) do
    GenServer.call(server, {:filename, filename})
  end

  def process(server, {:part2_string, string}) do
    GenServer.call(server, {:part2_string, string})
  end

  def process(server, {:part2_filename, filename}) do
    GenServer.call(server, {:part2_filename, filename})
  end

  ## PRIVATE FUNCTIONS

  defp read(filename) do
    {:ok, string} = File.read(filename)

    string
  end

  defp task1_file(filename) do
    filename
    |> read()
    |> task1()
  end

  defp task1(string) do
    string
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&parse/1)
    |> List.flatten()
    |> Enum.reduce({1, 1, 0}, &f/2)
    |> Tuple.to_list()
    |> Enum.at(2)
  end

  defp task2_file(filename) do
    filename
    |> read()
    |> task2()
  end

  defp task2(string) do
    string
    |> String.trim()
    |> String.split("\n")
    |> Enum.map(&parse/1)
    |> List.flatten()
    |> Enum.reduce([x: 1, row: 0, crt: ""], &g/2)
    |> Enum.at(2)
    |> Tuple.to_list()
    |> Enum.at(1)
  end

  defp parse("noop") do
    [0]
  end

  defp parse("addx " <> value) do
    [0, String.to_integer(value)]
  end

  defp f(value, {x, line, sum}) do
    if(rem(line - 20, 40) == 0) do
      sum = sum + line * x

      {x + value, line + 1, sum}
    else
      {x + value, line + 1, sum}
    end
  end

  defp g(value, x: x, row: row, crt: crt) do
    crt = newcrt(row, x, crt)

    [x: x + value, row: rem(row + 1, 40), crt: crt]
  end

  defp newcrt(row, x, crt) when row >= x - 1 and row <= x + 1 do
    crt <> "#" <> linejump(row)
  end

  defp newcrt(row, _, crt) do
    crt <> "." <> linejump(row)
  end

  defp linejump(39), do: "\n"
  defp linejump(_), do: ""
end
