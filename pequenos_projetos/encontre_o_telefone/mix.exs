defmodule EncontreOTelefone.MixProject do
  use Mix.Project

  def project do
    [
      app: :encontre_o_telefone,
      version: "0.1.0",
      elixir: "~> 1.13",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      test_coverage: [tool: ExCoveralls]

    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],

          app: :excoveralls,
          version: "1.0.0",
          elixir: "~> 1.0.0",
          deps: deps(),
          test_coverage: [tool: ExCoveralls],
          preferred_cli_env: [
            coveralls: :test,
            "coveralls.detail": :test,
            "coveralls.post": :test,
            "coveralls.html": :test
          ],
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:excoveralls, "~> 0.10", only: :test}

      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
