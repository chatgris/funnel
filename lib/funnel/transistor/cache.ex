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
    {:ok, Funnel.Transistor.CacheState.new(max: max)}
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

  def handle_cast({:push, item}, state) do
    {:noreply, state.update(index: state.index + 1, items: new_items(state, item))}
  end

  def handle_call({:list, last_id}, _from, state) do
    {:reply, Enum.reverse(selected(state.items, last_id)), state}
  end

  defp selected(cache, from) do
    Enum.filter(cache, fn(item) -> item[:id] > from end)
  end

  defp new_items(state, item) do
    [[id: state.index, item: item] | Enum.take(state.items, state.max - 1)]
  end
end
