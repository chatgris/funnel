defmodule Funnel.Mixfile do
  use Mix.Project

  def project do
    [ app: :funnel,
      version: "0.4.1",
      compilers: [:elixir, :app],
      compile_path: "tmp/#{Mix.env}/funnel/ebin",
      elixir: "~> 1.2",
      description: description,
      package: package,
      consolidate_protocols: Mix.env == :prod,
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ applications: [:httpoison, :poison, :logger],
      mod: { Funnel, [] } ]
  end

  defp deps do
    [ { :httpoison,      "~> 0.9" },
      { :poison,         "~> 2.1" },
      { :uuid,           "~> 1.1" },
      { :ex_doc,         "~> 0.5", only: :dev },
      { :poolboy,        "~> 1.5" }
    ]
  end

  defp description do
    """
    Streaming API built upon ElasticSearch's percolation.
    """
  end

  defp package do
    [
      contributors: ["chatgris"],
      licenses: ["MIT"],
      links: %{"Github" => "https://github.com/chatgris/funnel"}
    ]
  end
end
