defmodule Funnel.Supervisor do
  @moduledoc """
  Supervise all the things. \o/
  Start the dynamo application and the percolators's pool
  """
  use Supervisor.Behaviour

  @doc """

  Start the dynamo application and the percolators's pool
  """
  def start_link(nil) do
    :supervisor.start_link(__MODULE__, nil)
  end

  @doc """

  Default values of `Funnel.Supervisor`.
  """
  def init(nil) do
    children = [
      worker(Funnel.PercolatorPool, []),
      supervisor(Funnel.Transistors, []),
      supervisor(Funnel.Dynamo, [])
    ]

    supervise children, strategy: :one_for_one
  end
end
