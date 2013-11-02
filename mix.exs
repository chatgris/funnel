defmodule Funnel.Mixfile do
  use Mix.Project

  def project do
    [ app: :funnel,
      version: "0.0.1",
      dynamos: [Funnel.Dynamo],
      compilers: [:elixir, :dynamo, :app],
      env: [prod: [compile_path: "ebin"]],
      compile_path: "tmp/#{Mix.env}/funnel/ebin",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ applications: [:cowboy, :dynamo, :httpotion],
      mod: { Funnel, [] } ]
  end

  defp deps do
    [ { :cowboy, github: "extend/cowboy" },
      { :httpotion, "0.2.2", [github: "myfreeweb/httpotion"]},
      { :exjson, git: "https://github.com/guedes/exjson.git" },
      { :dynamo, "0.1.0-dev", github: "elixir-lang/dynamo" } ]
  end
end
