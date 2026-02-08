defmodule Spitfire.MixProject do
  use Mix.Project

  @source_url "https://github.com/elixir-tools/spitfire"

  def project do
    [
      app: :spitfire,
      description: "Error resilient parser for Elixir",
      version: "0.3.3",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: [main: "Spitfire"],
      package: package(),
      dialyzer: [
        plt_core_path: "priv/plts",
        plt_local_path: "priv/plts",
        ignore_warnings: ".dialyzer_ignore.exs"
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, ">= 0.0.0", only: :dev},
      {:styler, "~> 0.11", only: [:dev, :test]},
      {:credo, "~> 1.7", only: :dev},
      {:dialyxir, "~> 1.0", only: :dev}

      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end

  defp package do
    [
      maintainers: ["Mitchell Hanberg"],
      licenses: ["MIT"],
      links: %{
        GitHub: @source_url,
        Sponsor: "https://github.com/sponsors/mhanberg"
      },
      files: ~w(lib src LICENSE mix.exs README.md .formatter.exs)
    ]
  end
end
