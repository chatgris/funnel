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
  def start_link(conn) do
    {:ok, cache} = Funnel.Transistor.Cache.start_link
    :gen_server.start_link({:local, name(conn)}, __MODULE__, cache, [])
  end

  @doc """

  Wrapper around `GenServer`. Notify a user of matches.

  * `token`    - User's token
  * `match`    - Filter's id
  * `body`     - Document in json
  """
  def notify(token, match, body) do
    :gen_server.cast binary_to_atom(token), {:notify, match, body}
  end

  @doc """

  Wrapper around `GenServer`. Add a new connection in the connections's pool.

  * `conn`    - Dynamo's connection
  """
  def add(conn) do
    conn = conn.send_chunked(200)
    :gen_server.call name(conn), {:add, conn}
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
    {:reply, conn, state.update(connections: [conn | state.connections])}
  end

  defp name(conn) do
    binary_to_atom(conn.params[:token])
  end

  defp write(acc, conn, response, id) do
    filter acc, conn.chunk "id:#{id}\n\ndata: #{response}\n\n"
  end

  defp filter(acc, {:ok, conn}) do
    [conn | acc]
  end

  defp filter(acc, {:error, _}) do
    acc
  end
end
