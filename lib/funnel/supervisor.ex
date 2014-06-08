defmodule Funnel.Supervisor do
  @moduledoc """
  Supervise all the things. \o/
  Start the dynamo application and the percolators's pool
  """
  use Supervisor

  @doc """
  Start the dynamo application and the percolators's pool
  """
  def start_link do
    Supervisor.start_link(__MODULE__, [])
  end

  @doc """
  Default values of `Funnel.Supervisor`.
  """
  def init(_options) do
    children = [
      worker(Funnel.PercolatorPool, []),
      supervisor(Funnel.Transistors, []),
      supervisor(Funnel.Caches, [])
    ]

    supervise(children, strategy: :one_for_one)
  end
end
