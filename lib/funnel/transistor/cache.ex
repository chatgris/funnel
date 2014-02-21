defmodule Funnel.Transistor.Cache do
  @moduledoc """
  `Funnel.Transistor.Cache` keeps the x last item received by a
  `Funnel.Transistor`, and can expose the whole list, or from a given id.
  """

  use GenServer.Behaviour
  alias Funnel.Transistor.CacheState

  @doc """

  Start a new `Funnel.Transistor` actor.
  """
  def start_link(token) do
    name(token)
      |> find_or_start
  end

  @doc """

  Default values of `Funnel.Transistor.Cache`. An empty list.
  """
  def init(nil) do
    max = case System.get_env("FUNNEL_CACHE_MAX_ITEMS") do
      nil -> 10
      max -> max
    end
    {:ok, CacheState.new(max: max)}
  end

  @doc """
  Push a new item into the list.

  * `pid`    - Cache store
  * `item`   - Pretty much anything
  """
  def push(pid, item) do
    :gen_server.call pid, {:push, item}
  end

  @doc """
  Returns the items present in cache.

  * `pid`    - Cache store
  * `from`   - Last-Event-Id
  """
  def list(pid, from) when is_binary(from) do
    from = binary_to_integer(from)
    :gen_server.call pid, {:list, from}
  end

  @doc """
  Returns the items present in cache.

  * `pid`    - Cache store
  * `from`   - Last-Event-Id
  """
  def list(pid, from) when is_integer(from) do
    :gen_server.call pid, {:list, from}
  end

  @doc """
  Returns the items present in cache.

  * `pid`    - Cache store
  * `from`   - Last-Event-Id
  """
  def list(pid, nil) do
    :gen_server.call pid, {:list, -1}
  end

  @doc """
  Returns the items present in cache.

  * `pid`    - Cache store
  """
  def list(pid) do
    :gen_server.call pid, {:list, -1}
  end

  def handle_call({:push, item}, _from, state) do
    state = state.update(index: state.index + 1)
    {:reply, state.index, state.update(items: new_items(state, item))}
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

  defp name(token) do
    binary_to_atom("#{token}_cache")
  end

  defp find_or_start(name) do
    case Process.whereis name do
      nil -> :gen_server.start_link({:local, name}, __MODULE__, nil, [])
      pid -> {:ok, pid}
    end
  end
end
