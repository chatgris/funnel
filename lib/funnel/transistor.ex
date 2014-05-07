defmodule Funnel.Transistor do
  @moduledoc """
  `Funnel.Transistor` can be see as a User. It can have several connections, and
  will notify a matching document on each of thoses connections.
  """
  use GenServer.Behaviour
  alias Funnel.Transistor.Cache
  alias Funnel.Caches
  alias Funnel.Transport

  defmodule Funnel.TransistorState do
    defstruct cache: nil, connections: []
  end

  alias Funnel.TransistorState

  @doc """

  Start a new `Funnel.Transistor` actor.
  """
  def start_link(token) do
    name(token)
      |> find_or_start
  end

  @doc """

  Wrapper around `GenServer`. Notify a user of matches.

  * `token`        - User's token
  * `id`           - Document's id
  * `response`     - Document in json
  """
  def notify(token, id, response) do
    case Process.whereis(name(token)) do
      pid -> :gen_server.cast pid, {:notify, id, response}
    end
  end

  @doc """

  Wrapper around `GenServer`. Add a new connection in the connections's pool.

  * `conn`    - Process id
  * `token`   - the token used to identify the connection
  * `last_id` - the last id received in the connection
  """
  def add(conn, token, last_id \\ nil) do
    :gen_server.call name(token), {:add, conn, last_id}
  end

  @doc """

  Default values of `Funnel.Transistor`. An empty pool of connections.
  """
  def init(cache) do
    {:ok, %TransistorState{cache: cache}}
  end

  @doc """

  Writes on each connections the matched document.
  """
  def handle_cast({:notify, id, item}, state) do
    Enum.each(state.connections, fn(conn) -> write(conn, message(id, item)) end)
    {:noreply, state }
  end

  @doc """

  Add a connection to the connections's pool.
  """
  def handle_call({:add, conn, last_id}, _from, state) do
    write_from_cache(conn, state.cache, last_id)
    {:reply, conn, Map.update!(state, :connections, fn(_) -> [conn | state.connections] end) }
  end

  defp name(token) do
    binary_to_atom(token)
  end

  defp find_or_start(name) do
    case Process.whereis name do
      nil -> boot(name)
      pid -> {:ok, pid}
    end
  end

  defp boot(name) do
    {:ok, cache} = Caches.add name
    :gen_server.start_link({:local, name}, __MODULE__, cache, [])
  end

  defp write_from_cache(conn, cache, last_id) do
    Enum.reduce(Cache.list(cache, last_id), conn, fn(item, conn) -> write(conn, item) end)
  end

  defp write(conn, item) do
    Transport.write(conn, item)
  end

  defp message(id, item) do
    [id: id, item: item]
  end
end
