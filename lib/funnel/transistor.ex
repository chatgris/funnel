defmodule Funnel.Transistor do
  @moduledoc """
  `Funnel.Transistor` can be see as a User. It can have several connections, and
  will notify a matching document on each of thoses connections.
  """
  use GenServer.Behaviour
  alias Funnel.Transistor.Cache
  alias Funnel.Caches

  defrecord Funnel.TransistorState, cache: nil, connections: []

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

  * `conn`    - Dynamo's connection
  * `token`   - the token used to identify the connection
  """
  def add(conn, token) do
    conn = conn.send_chunked(200)
    :gen_server.call name(token), {:add, conn}
  end

  @doc """

  Default values of `Funnel.Transistor`. An empty pool of connections.
  """
  def init(cache) do
    { :ok, TransistorState.new(cache: cache) }
  end

  @doc """

  Writes on each connections the matched document.
  """
  def handle_cast({:notify, id, response}, state) do
    connections = Enum.reduce(state.connections, [], fn(conn, connections) -> write(connections, conn, response, id) end)
    {:noreply, state.update(connections: connections) }
  end

  @doc """

  Add a connection to the connections's pool.
  """
  def handle_call({:add, conn}, _from, state) do
    conn = write_from_cache(conn, state.cache)
    {:reply, conn, state.update(connections: [conn | state.connections])}
  end

  defp name(token) do
    binary_to_atom(token)
  end

  defp write(connections, conn, response, id) do
    result_write connections, conn.chunk message(id, response)
  end

  defp result_write(connections, {:ok, conn}) do
    [conn | connections]
  end

  defp result_write(connections, {:error, _}) do
    connections
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

  defp write_from_cache(conn, cache) do
    Enum.reduce(Cache.list(cache, conn.params[:last_id]), conn, &write/2)
  end

  defp write(item, conn) do
    {:ok, conn} = conn.chunk message(item[:id], item[:item])
    conn
  end

  defp message(id, body) do
    "id:#{id}\ndata: #{body}\n\n"
  end
end
