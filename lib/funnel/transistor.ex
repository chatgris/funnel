defmodule Funnel.Transistor do
  @moduledoc """
  `Funnel.Transistor` can be see as a User. It can have several transports, and
  will notify a matching document on each of thoses transports.
  """
  use GenServer.Behaviour
  alias Funnel.Transistor.Cache
  alias Funnel.Caches
  import Funnel.Transport, only: [write: 2]

  defmodule Funnel.TransistorState do
    defstruct cache: nil, transports: []
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

  Wrapper around `GenServer`. Add a new transport in the transports's pool.

  * `transport`    - Something implementing the `Funnel.Transport` protocol
  * `token`   - the token used to identify the transport
  * `last_id` - the last id received in the transport
  """
  def add(transport, token, last_id \\ nil) do
    :gen_server.call name(token), {:add, transport, last_id}
  end

  @doc """

  Default values of `Funnel.Transistor`. An empty pool of transports.
  """
  def init(cache) do
    {:ok, %TransistorState{cache: cache}}
  end

  @doc """

  Writes on each transports the matched document.
  """
  def handle_cast({:notify, id, response}, state) do
    connections = Enum.reduce(state.connections, [], fn(conn, connections) -> write(connections, conn, response, id) end)
    {:noreply, %TransistorState{state | connections: connections}}
  end

  @doc """

  Add a transport to the transports's pool.
  """
  def handle_call({:add, conn, last_id}, _from, state) do
    conn = write_from_cache(conn, state.cache, last_id)
    {:noreply, %TransistorState{state | connections: [conn | state.connections]}}
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

  defp write_from_cache(transport, cache, last_id) do
    Enum.reduce(Cache.list(cache, last_id), transport, fn(item, transport) ->
      {:ok, transport} = write(transport, item)
      transport
    end)
  end

  defp message(id, item) do
    %{:id => id, :item => item}
  end
end
