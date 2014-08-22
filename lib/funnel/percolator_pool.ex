defmodule Funnel.PercolatorPool do
  @moduledoc """
  Pool of `Funnel.Percolators`.
  """
  use Supervisor
  import Funnel.Percolator, only: [percolate: 3]

  @doc """

  Start the percolators's pool
  """
  def start_link do
    Supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    pool_options = [
      name: {:local, :percolator_pool},
      worker_module: Funnel.Percolator,
      size: size,
      max_overflow: max_overflow
    ]

    children = [
      :poolboy.child_spec(:percolator_pool, pool_options)
    ]

    supervise(children, strategy: :one_for_one)
  end

  @doc """

  Submit a document to Elasticsearch's percolator through the pool.
  """
  def percolate(index_id, body) do
    :poolboy.transaction(:percolator_pool, fn(percolator)-> percolate(percolator, index_id, body) end)
  end

  defp size do
    Application.get_env(:funnel, :percolator_pool_size)
  end

  defp max_overflow do
    Application.get_env(:funnel, :percolator_pool_max_overflow)
  end
end
