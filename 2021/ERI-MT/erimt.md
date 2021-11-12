# Minicurso na ERI-MT 2021

## Elixir, a linguagem de programação brasileira de código aberto utilizada no mundo todo




## Sobre o palestrante


Adolfo Neto é professor da UTFPR, criador e co-host do Emílias Podcast - Mulheres na Computação e membro do projeto Emílias - Armação em Bits.

- Twitter https://twitter.com/adolfont
-  Erlang Ecosystem Foundation https://erlef.org/wg/education
- Elixir em Foco https://anchor.fm/elixiremfoco/
- Elixir_UTFPR https://twitter.com/elixir_utfpr
- YouTube https://www.youtube.com/c/AdolfoNeto

## Resumo

Uma breve introdução à linguagem Elixir. Vamos conhecer as principais características da linguagem e porque ela é boa até mesmo para iniciantes em programação.

<!-- livebook:{"break_markdown":true} -->

### O que é Elixir?

* [Site de Elixir](https://elixir-lang.org/)
* [Página na Wikipédia sobre Elixir](https://pt.wikipedia.org/wiki/Elixir_(linguagem_de_programa%C3%A7%C3%A3o))

### Elixir é mesmo uma linguagem brasileira?

Foi criada por José Valim em 2011-12. Mas isto é o suficiente para dizer que ela é brasileira?

### Vagas 

[100 mil dólares](https://twitter.com/elixir_utfpr/status/1458078709487915017?s=20)


### É mesmo usada no mundo todo? Em produção?

* Escutem a série Elixir em Produção do podcast [Elixir em Foco](https://anchor.fm/elixiremfoco/)
  * Um dos [6 podcasts ativos relacionados a Elixir ou á BEAM](https://elixirschool.com/en/podcasts)
* Elixir é a [quarta linguagem com melhor fator amada/temida da pesquisa com usuários do StackOverflow de 2021](https://insights.stackoverflow.com/survey/2021#technology-most-loved-dreaded-and-wanted) e está em [posição 17 entre as mais desejadas](https://insights.stackoverflow.com/survey/2021#most-loved-dreaded-and-wanted-language-want).
* Na mesma pesquisa, é a [terceira que paga melhor](https://insights.stackoverflow.com/survey/2021#technology-top-paying-technologies).
* Empresas que usam Elixir [1](https://serokell.io/blog/elixir-in-production), 
  [2](https://dashbit.co/blog/ten-years-ish-of-elixir), [3](https://elixir-lang.org/cases.html), [4](https://www.hostgator.com.br/blog/elixir-linguagem-programacao-brasileira/), [5](https://github.com/elixirbrasil/empresas)
* [Empregos para Devs Elixir](https://elixir-radar.com/jobs)

### É o assunto de várias conferências!

* ElixirConf: [Europe](https://www.elixirconf.eu/), [USA/World](https://www.elixirconf.com/), [Africa](https://elixirconf.africa/)
* Code BEAM. A mais recente foi a [Code BEAM America 2021](https://codesync.global/conferences/code-beam-sf-2021/)
* [Elixir Brasil](https://twitter.com/elixir_brasil) - 27 e 28 de novembro de 2021
* [Code BEAM BR](https://www.codebeambr.com/)

## Isto aqui é o Livebook!

Tudo o que vocês estão vendo está sendo executado em uma instalação de Livebook, um software para a criação de nobebooks interativos com código em Elixir, dados e documentação.

* Repositório de código aberto: https://github.com/livebook-dev/livebook
* Site: http://livebook.dev


## As melhores características de Elixir

## Funções na computação

## Funções nomeadas em módulos

```elixir
defmodule Saude do
  def calcula_imc(peso, altura) do
    peso / (altura * altura)
  end
end
```

```elixir
Saude.calcula_imc(100, 1.5)
```

## Funções anônimas

```elixir
fn peso, altura -> peso / (altura * altura) end
```

```elixir
imc = fn peso, altura -> peso / (altura * altura) end
imc.(100, 1.5)
```

```elixir
&(&1 / (&2 * &2))
```

```elixir
imc = &(&1 / (&2 * &2))
imc.(100, 1.5)
```

### Funções matemáticas (puras)

Para as mesmas entradas, sempre retorna as mesmas saídas.

Não alteram o estado geral do sistema.

### Não-funções matemáticas (funções impuras)

Para as mesmas entradas, a cada execução pode retornar uma saída diferente.

Exemplo:

```elixir
Enum.shuffle([1, 2, 3])
```

Ou alteram o estado geral do sistema.

## A BEAM

[Introduction to Erlang](https://serokell.io/blog/introduction-to-erlang)

## Casamento de padrões (pattern matching)

## Sintaxe amigável (ex. omissão de parênteses)

```elixir
Enum.take(Stream.map(1..1_000_000, &(&1 * &1)), 10)
```

## O Operador Pipe

```elixir
1..1_000_000
|> Stream.map(&(&1 * &1))
|> Enum.take(100)
|> Enum.filter(fn x -> rem(x, 21) == 0 end)
|> Enum.join(", ")
```

```elixir
[65, 66, 67]
```

## Funções de Ordem Superior

```elixir
f = fn x -> fn y -> x * y end end
```

```elixir
g = f.(3)
```

```elixir
g.(4)
```

## Final

### Acompanhe a comunidade

* [Elixir Brasil 2021](https://twitter.com/elixir_brasil/)
* https://twitter.com/elixir_utfpr
* https://twitter.com/adolfont
* https://twitter.com/elixiremfoco
* https://www.youtube.com/c/AdolfoNeto
* [Elixir, Erlang and the BEAM with Adolfo Neto](https://www.youtube.com/channel/UC6ETZk7tlYJzfRz-zS9B6xw)
* [Erlang Ecosystem Foundation](https://erlef.org/)
* Telegram (Elixir Brasil, ELUG_CE)

### Links do dia

* https://github.com/jonatanklosko
* https://colab.research.google.com/
* https://github.com/elixir-lang/elixir
* https://en.wikipedia.org/wiki/TypeScript
* https://anchor.fm/osprogramadores
* https://www.lua.org/about.html
* https://elixir-lang.org/cases.html
* https://exercism.org/
* WSL https://www.google.com/search?q=WSL+windows&sxsrf=AOaemvLnsz1bcgvMX3ICeDSU9BZjzCmUbQ%3A1636748502185&ei=1syOYdzTCv7Q1sQPyfCPqAs&oq=WSL+windows&gs_lcp=Cgdnd3Mtd2l6EAMyBAgAEEMyBQgAEMsBMgUIABDLATIECAAQQzIFCAAQywEyBQgAEMsBMgUIABDLATIFCAAQywEyBQgAEMsBMgUIABDLAToHCAAQRxCwAzoECCMQJzoECC4QQzoKCAAQgAQQhwIQFEoECEEYAFClBVjAEGD3EGgAcAN4AIAB4QKIAdENkgEFMi00LjKYAQCgAQHIAQjAAQE&sclient=gws-wiz&ved=0ahUKEwjclryo05P0AhV-qJUCHUn4A7UQ4dUDCA4&uact=5
* https://dev.to/guilherme44/pt-br-instalando-elixir-com-asdf-277h
* https://www.beecrowd.com.br/judge/en/login
* https://github.com/groxio-learning/grapevine
* https://www.phoenixframework.org/blog/build-a-real-time-twitter-clone-in-15-minutes-with-live-view-and-phoenix-1-5

* https://t.me/elixirbr
* https://t.me/elug_ce
* https://twitter.com/search?q=%23MyElixirStatus&src=typed_query&f=live
* https://www.sympla.com.br/semana-da-ciencia-da-computacao-e-tecnologia-da-informacao--scti-2021__1263117
* https://github.com/pilhacheia/tretadev/issues/85
* https://elixirforum.com/t/which-code-editor-or-ide-do-you-use/78/14
