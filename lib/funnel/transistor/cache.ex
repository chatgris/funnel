defmodule Funnel.Transistor.Cache do
  @moduledoc """
  `Funnel.Transistor.Cache` keeps the x last item received by a
  `Funnel.Transistor`, and can expose the whole list, or from a given id.
  """

  use GenServer.Behaviour

  @doc """

  Start a new `Funnel.Transistor` actor.
  """
  def start_link do
    :gen_server.start_link(__MODULE__, [], [])
  end

  @doc """

  Default values of `Funnel.Transistor.Cache`. An empty list.
  """
  def init([]) do
    max = case System.get_env("FUNNEL_CACHE_MAX_ITEMS") do
      nil -> 10
      max -> max
    end
    {:ok, {0, [], max}}
  end

  @doc """
  Push a new item into the list.

  * `pid`    - Cache store
  * `item`   - Pretty much anything
  """
  def push(pid, item) do
    :gen_server.cast pid, {:push, item}
  end

  @doc """
  Returns the items present in cache.

  * `pid`    - Cache store
  * `from`   - Last-Event-Id
  """
  def list(pid, from \\ -1) do
    :gen_server.call pid, {:list, from}
  end

  def handle_cast({:push, item}, {index, cache, max}) do
    {:noreply, {index + 1, [[id: index, item: item] | Enum.take(cache, max - 1)], max}}
  end

  def handle_call({:list, last_id}, _from, {index, cache, max}) do
    {:reply, Enum.reverse(selected(cache, last_id)), {index, cache, max}}
  end

  defp selected(cache, from) do
    Enum.filter(cache, fn(item) -> item[:id] > from end)
  end
end
