defmodule Funnel.Transistors do
  @moduledoc """
  Supervise all `Funnel.Transistors`.
  """
  use Supervisor.Behaviour

  @doc """
  Start the Transistors's supervisor.
  """
  def start_link do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  @doc """
  Default values of `Funnel.Transistors`.
  """
  def init([]) do
    children = [worker(Funnel.Transistor, [])]

    supervise children, strategy: :simple_one_for_one
  end

  @doc """
  Start a `Funnel.Transistor` under the supervision tree.
  """
  def add(token) do
    :supervisor.start_child(__MODULE__, [token])
  end
end
