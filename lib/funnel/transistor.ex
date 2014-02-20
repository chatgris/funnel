defmodule Funnel.Transistor do
  @moduledoc """
  `Funnel.Transistor` can be see as a User. It can have several connections, and
  will notify a matching document on each of thoses connections.
  """
  use GenServer.Behaviour
  alias Funnel.Transistor.Cache
  alias Funnel.TransistorState

  @doc """

  Start a new `Funnel.Transistor` actor.
  """
  def start_link(token) do
    find(name(token))
  end

  @doc """

  Wrapper around `GenServer`. Notify a user of matches.

  * `token`    - User's token
  * `match`    - Filter's id
  * `body`     - Document in json
  """
  def notify(token, match, body) do
    {:ok, pid} = find(binary_to_atom(token))
    :gen_server.cast pid, {:notify, match, body}
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
  def handle_cast({:notify, match, body}, state) do
    {:ok, response} = JSEX.encode([filter_id: match, body: body])
    id = Cache.push(state.cache, response)
    connections = Enum.reduce(state.connections, [], fn(conn, acc) -> write(acc, conn, response, id) end)
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

  defp write(acc, conn, response, id) do
    filter acc, conn.chunk message(id, response)
  end

  defp filter(acc, {:ok, conn}) do
    [conn | acc]
  end

  defp filter(acc, {:error, _}) do
    acc
  end

  defp find(name) do
    case Process.whereis name do
      nil -> boot(name)
      pid -> {:ok, pid}
    end
  end

  defp boot(name) do
    {:ok, cache} = Funnel.Transistor.Cache.start_link
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
