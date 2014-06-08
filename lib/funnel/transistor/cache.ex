defmodule Funnel.Transistor.Cache do
  @moduledoc """
  `Funnel.Transistor.Cache` keeps the x last item received by a
  `Funnel.Transistor`, and can expose the whole list, or from a given id.
  """

  use GenServer

  defmodule Funnel.Transistor.CacheState do
    defstruct items: [], max: 10
  end

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
    {:ok, %CacheState{max: max}}
  end

  @doc """
  Push a new item into the list.

  * `pid`    - Cache store
  * `id`     - The id of the item
  * `item`   - Pretty much anything
  """
  def push(pid, id, item) do
    GenServer.call(pid, {:push, id, item})
  end

  @doc """
  Returns the items present in cache.

  * `pid`    - Cache store
  * `from`   - Last-Event-Id
  """
  def list(pid, from) do
    GenServer.call(pid, {:list, from})
  end

  @doc """
  Returns the items present in cache.

  * `pid`    - Cache store
  """
  def list(pid) do
    GenServer.call(pid, {:list, nil})
  end

  def handle_call({:push, id, item}, _from, state) do
    {:reply, id, %CacheState{state | items: new_items(state, id, item) }}
  end

  def handle_call({:list, last_id}, _from, state) do
    {:reply, filter(Enum.reverse(state.items), last_id), state}
  end

  defp new_items(state, id, item) do
    [%{:id => id, :item => item} | Enum.take(state.items, state.max - 1)]
  end

  defp name(token) do
    binary_to_atom("#{token}_cache")
  end

  defp filter(items, nil) do
    items
  end

  defp filter(items, id) do
    {_status, new_items} = Enum.reduce(items, {false, []}, fn(item, acc) -> filter_match(item, id, acc) end)
    new_items
  end

  defp filter_match(item, _id, {true, items}) do
    {true, [item | items]}
  end

  defp filter_match(%{:id => id_item, :item => _item}, id, _) when id_item == id do
    {true, []}
  end

  defp filter_match(_item, _id, {status, items}) do
    {status, items}
  end

  defp find_or_start(name) do
    case Process.whereis name do
      nil -> :gen_server.start_link({:local, name}, __MODULE__, nil, [])
      pid -> {:ok, pid}
    end
  end
end
