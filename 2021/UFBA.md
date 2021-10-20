# Palestra na Universidade Federal da Bahia (UFBA)

## Elixir, a linguagem de programação brasileira utilizada no mundo todo

[![Execute em uma Instalação de Livebook](https://i.ibb.co/XYcR1dy/Execute-em-uma-instalacao-do-Livebook.png)](https://livebook.dev/run?url=https%3A%2F%2Fgithub.com%2Fadolfont%2Fpensandoemelixir%2Fblob%2Fmain%2F2021%2FUFBA.md)

<!-- UFBAUTFPR2021 adolfont.fly.dev -->

* Quando? 20/10/2021 (QUI) 13:00-15:00

* Onde? Informação privada.

* Página da disciplina: [MATA56 - Paradigmas de Linguagens de Programação](https://github.com/mata56-ic-ufba/paradigmas)

## Sobre o palestrante

Adolfo Neto é professor da UTFPR, criador e co-host do Emílias Podcast - Mulheres na Computação e membro do projeto Emílias - Armação em Bits.

## Resumo

Uma breve introdução à linguagem Elixir. Vamos conhecer as principais características da linguagem e porque ela é boa até mesmo para iniciantes em programação.

<!-- livebook:{"break_markdown":true} -->

### O que é Elixir?

* [Site de Elixir](https://elixir-lang.org/)
* [Página na Wikipédia sobre Elixir](https://pt.wikipedia.org/wiki/Elixir_(linguagem_de_programa%C3%A7%C3%A3o))

### Elixir é mesmo uma linguagem brasileira?

Foi criada por José Valim em 2011-12. Mas isto é o suficiente para dizer que ela é brasileira?

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
* Code BEAM. A próxima será a [Code BEAM America 2021](https://codesync.global/conferences/code-beam-sf-2021/)
* [Elixir Brasil](https://twitter.com/elixir_brasil) - 27 e 28 de novembro de 2021
* [Code BEAM BR](https://www.codebeambr.com/)

## Isto aqui é o Livebook!

Um software para a criação de nobebooks interativos com código em Elixir,dados e documentação.

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

### Funções matemáticas (puras)

### Não-funções matemtáticas (funções impuras)

## A BEAM

[Introduction to Erlang](https://serokell.io/blog/introduction-to-erlang)

## Casamento de padrões (pattern matching)

## Sintaxe amigável (ex. omissão de parênteses)

## O Operador Pipe

## Funções de Ordem Superior

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
