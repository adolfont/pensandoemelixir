my_fun = fn x -> x * 4 end
my_fun_2 = fn x, y -> x - y + 1000 end

[fn x -> x * 2 end, fn y -> y + 5 end, &(&1 * &1), fn y -> y - 2 end, fn _ -> 1 end]
|> IO.inspect()
|> Enum.map(fn f -> &my_fun.(f.(&1)) end)
|> IO.inspect()
|> Enum.map(fn x -> x.(3) end)
|> IO.inspect()
|> Enum.sum()
|> IO.inspect()
|> then(fn x -> my_fun_2.(13, x) end)
|> IO.inspect()
|> then(fn x -> :math.pow(3, x - 300) end)
|> IO.inspect()
