input = """
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
"""

chunk_fun = fn element, acc ->
  if element == "" do
    {:cont, Enum.reverse(acc), []}
  else
    {:cont, [element | acc]}
  end
end

after_fun = fn
  [] -> {:cont, []}
  acc -> {:cont, Enum.reverse(acc), []}
end

max_list = fn list ->
  Enum.map(list, &String.to_integer/1)
  |> Enum.sum()
end

# PART 1

input
|> String.trim()
|> String.split("\n")
|> Enum.map(&String.trim/1)
|> Enum.chunk_while([], chunk_fun, after_fun)
|> Enum.max_by(max_list)
|> Enum.map(&String.to_integer/1)
|> Enum.sum()
|> IO.inspect()

# PART 2
input
|> String.trim()
|> String.split("\n")
|> Enum.map(&String.trim/1)
|> Enum.chunk_while([], chunk_fun, after_fun)
|> Enum.sort_by(max_list)
|> Enum.reverse()
|> Enum.take(3)
|> List.flatten()
|> Enum.map(&String.to_integer/1)
|> Enum.sum()
|> IO.inspect()
