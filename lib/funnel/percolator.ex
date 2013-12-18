defmodule Funnel.Percolator do
  use GenServer.Behaviour

  def start_link do
    :gen_server.start_link({:local, :percolator}, __MODULE__, nil, [])
  end

  def percolate(body) do
    :gen_server.cast :percolator, {:percolate, body}
  end

  def init do
    {:ok, nil}
  end

  def handle_cast({:percolate, body}, nil) do
    Funnel.Es.percolate body
    { :noreply, nil}
  end
end
