defmodule Funnel.Mixfile do
  use Mix.Project

  def project do
    [ app: :funnel,
      version: "0.0.1",
      dynamos: [Funnel.Dynamo],
      compilers: [:elixir, :dynamo, :app],
      env: [prod: [compile_path: "ebin"]],
      compile_path: "tmp/#{Mix.env}/funnel/ebin",
      elixir: "~> 0.12",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ applications: [:cowboy, :dynamo, :httpotion],
      mod: { Funnel, [] } ]
  end

  defp deps do
    [ { :cowboy,      github: "extend/cowboy" },
      { :httpotion,   github: "chatgris/httpotion" },
      { :jsex,        github: "igas/jsex", branch: "elixir13" },
      { :dynamo,      github: "dynamo/dynamo", branch: "0.13" },
      { :uuid,        github: "travis/erlang-uuid" },
      { :ex_doc,      github: "elixir-lang/ex_doc" },
      { :poolboy,     github: "devinus/poolboy"}
    ]
  end
end
