defmodule Funnel.Mixfile do
  use Mix.Project

  def project do
    [ app: :funnel,
      version: "0.4.1",
      compilers: [:elixir, :app],
      compile_path: "tmp/#{Mix.env}/funnel/ebin",
      elixir: "~> 0.15 or 1.0.0-rc1 or 1.0.0",
      description: description,
      package: package,
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ applications: [:httpoison, :poison, :logger],
      mod: { Funnel, [] } ]
  end

  defp deps do
    [ { :httpoison,   "~> 0.3" },
      { :poison,      "~> 1.0" },
      { :uuid,        "~> 0.1.3" },
      { :ex_doc,      "~> 0.5", only: [:dev] },
      { :poolboy,     "~> 1.2" }
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
