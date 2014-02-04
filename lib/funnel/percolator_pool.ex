defmodule Funnel.PercolatorPool do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    # Here are my pool options
    pool_options = [
      name: {:local, :percolator_pool},
      worker_module: Funnel.Percolator,
      size: 20,
      max_overflow: 40
    ]

    children = [
      :poolboy.child_spec(:percolator_pool, pool_options)
    ]

    supervise(children, strategy: :one_for_one)
  end

  def percolate(index_id, body) do
    :poolboy.transaction(:percolator_pool, fn(percolator)-> Funnel.Percolator.percolate(percolator, index_id, body) end)
  end
end
