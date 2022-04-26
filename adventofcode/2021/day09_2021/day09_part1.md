<!-- livebook:{"autosave_interval_s":30} -->

# Advent of Code Day 09 - Valim's solution

## Part 1

https://adventofcode.com/2021/day/9

## Setup

```elixir
Mix.install([
  {:kino, "~> 0.4.1"}
])
```

```elixir
input = Kino.Input.textarea("Por favor, cole aqui sua entrada:")
```

<!-- livebook:{"reevaluate_automatically":true} -->

```elixir
lines =
  input
  |> Kino.Input.read()
  |> String.split()

matrix =
  for {line, row_number} <- Enum.with_index(lines),
      {value, col_number} <- Enum.with_index(String.to_charlist(line)),
      into: %{} do
    {{row_number, col_number}, value - ?0}
  end

Enum.filter(
  matrix,
  fn {{row_number, col_number}, value} ->
    up = matrix[{row_number - 1, col_number}]
    down = matrix[{row_number + 1, col_number}]
    left = matrix[{row_number, col_number - 1}]
    right = matrix[{row_number, col_number + 1}]

    value < up and
      value < down and
      value < left and
      value < right
  end
)
|> Enum.map(fn {{_, _}, value} -> value + 1 end)
|> Enum.sum()
```

```elixir
Enum.with_index(["Adolfo", "Elixir em Foco", "Elixir Outlaws"])
```

## Exemplo de uso do for

```elixir
for i <- 1..10, do: i * i * i
```

```elixir
for palavra <- ["Adolfo", "Elixir em Foco", "Elixir Outlaws"] do
  palavra
  |> String.codepoints()
  |> Enum.reverse()
  |> Enum.join()
end
```

```elixir
# Produto cartesiano
for x <- [1, 4, 5],
    y <- ["a", "b", "d", "f"] do
  {x, y}
end
```

## String.codepoints/1

```elixir
String.codepoints("Adolfo")
```

## Toda função em Elixir retorna algum valor

## Section

```elixir
IO.puts("Adolfo")
```

## String.to_charlist/1

```elixir
{String.to_charlist("01Adolfo"), [1200 | String.to_charlist("01Adolfo")]}
```

## Uso do operador "?"

```elixir
{?0, ?9, ?A, ?-}
```

## Kernel.min/2

```elixir
{min(2, 3), min(3, 2), min(2, nil), min(nil, 2), min(nil, nil), min("Adolfo", 45)}
```
