defmodule Funnel.Mixfile do
  use Mix.Project

  def project do
    [ app: :funnel,
      version: "0.0.1",
      compilers: [:elixir, :app],
      compile_path: "tmp/#{Mix.env}/funnel/ebin",
      elixir: "~> 0.14",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ applications: [:httpotion, :jsex],
      mod: { Funnel, [] } ]
  end

  defp deps do
    [ { :httpotion,   github: "myfreeweb/httpotion" },
      { :jsex,        "~> 2.0" },
      { :uuid,        github: "travis/erlang-uuid" },
      { :ex_doc,      github: "elixir-lang/ex_doc", only: [:dev] },
      { :poolboy,     "~> 1.2" }
    ]
  end
end
