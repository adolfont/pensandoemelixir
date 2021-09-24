# ElixirKenya - 24/09/2021

## Anonymous Functions

```elixir
fn x -> x + 1 end
```

```elixir
(fn x -> x + 1 end).(3)
```

```elixir
f = fn x -> x + 1 end
f.(3)
```

```elixir
&(&1 + 1)
```

```elixir
(&(&1 + 1)).(3)
```

```elixir
f = &(&1 + 1)
f.(3)
```

```elixir
(&(&1 * &2)).(4, 5)
```

## Not a mathematical function

```elixir
Enum.shuffle([1, 2, 3])
```

```elixir
:random.uniform()
```

## Higher-order functions

```elixir
f = fn x -> x + 1 end
list = [1, 2, 3]

Enum.map(list, f)
# new_list = [f(1), f(2), f(3)]
```

```elixir
defmodule Higher do
  def f do
    fn x -> x + 1 end
  end
end

list = [1, 2, 3]
Enum.map(list, Higher.f())
```

## State as a parameter

```elixir
# state
# {result, state} = f(23, state)
# {result, state} = f(34, state)

f = fn x, y -> [y | x] end

[]
|> f.(3)
|> f.(4)
|> f.(5)
```

## Currying?

```elixir
f = fn x, y -> x + y end

f.(2, 3)

g = f.(2)
g.(3)
```

## Adding a new element to the beginning of a list

```elixir
list = [1, 2, 3]

[4 | list]

list ++ [4]
```
