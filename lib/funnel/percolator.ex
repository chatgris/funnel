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
    Funnel.Es.percolate(body)
      |> Enum.each(fn(match)-> notify(match, body) end)
    { :noreply, nil}
  end

  defp notify(match, body) do
    [token, uuid] = String.split(match, "-")
    Funnel.Transistor.notify(token, uuid, body)
  end
end
