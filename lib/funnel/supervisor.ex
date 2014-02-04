defmodule Funnel.Supervisor do
  use Supervisor.Behaviour

  def start_link(nil) do
    :supervisor.start_link(__MODULE__, nil)
  end

  def init(nil) do
    children = [
      worker(Funnel.Percolator, []),
      supervisor(Funnel.Dynamo, [])
    ]

    supervise children, strategy: :one_for_one
  end
end
