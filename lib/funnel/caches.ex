defmodule Funnel.Caches do
  @moduledoc """
  Supervise all `Funnel.Caches`.
  """
  use Supervisor.Behaviour

  @doc """
  Start the Caches's supervisor.
  """
  def start_link do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  @doc """
  Default values of `Funnel.Caches`.
  """
  def init([]) do
    children = [worker(Funnel.Transistor.Cache, [])]

    supervise children, strategy: :simple_one_for_one
  end

  @doc """
  Start a `Funnel.Cache` under the supervision tree.
  """
  def add(token) do
    :supervisor.start_child(__MODULE__, [token])
  end
end
