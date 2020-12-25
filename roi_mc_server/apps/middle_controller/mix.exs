defmodule MiddleController.MixProject do
  use Mix.Project

  def project do
    [
      app: :middle_controller,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.9",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {MiddleController.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:httpoison, "~> 1.6"},
      {:kunerauqs, path: "R:/SkyDrive/R/16/NewJ2013/NEW_WORLD/20xx/new_era/kunerauqs_toolbox"},
      {:kun_fast_cgi,
       path: "R:/SkyDrive/R/16/NewJ2013/NEW_WORLD/20xx/new_era/elixir_fast_cgi/elixir"}
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"},
      # {:sibling_app_in_umbrella, in_umbrella: true}
    ]
  end
end
