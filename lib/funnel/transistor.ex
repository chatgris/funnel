defmodule Funnel.Transistor do
  @moduledoc """
  `Funnel.Transistor` can be see as a User. It can have several transports, and
  will notify a matching document on each of thoses transports.
  """
  use GenServer
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
    token
      |> extract_name
      |> find_or_start
  end

  @doc """
  Wrapper around `GenServer`. Notify a user of matches.

  * `token`        - User's token
  * `id`           - Document's id
  * `response`     - Document in json
  """
  def notify(token, id, response) do
    case Process.whereis(extract_name(token)) do
      pid -> GenServer.cast(pid, {:notify, id, response})
    end
  end

  @doc """
  Wrapper around `GenServer`. Add a new transport in the transports's pool.

  * `transport`    - Something implementing the `Funnel.Transport` protocol
  * `token`        - the token used to identify the transport
  * `last_id`      - the last id received in the transport
  """
  def add(transport, token, last_id \\ nil) do
    token
      |> extract_name
      |> GenServer.call({:add, transport, last_id})
  end

  @doc """
  Default values of `Funnel.Transistor`. An empty pool of transports.

  * `pid`    - Cache store
  """
  def init(cache) do
    {:ok, %TransistorState{cache: cache}}
  end

  @doc """
  Writes on each transports the matched document.
  """
  def handle_cast({:notify, id, item}, state) do
    transports = Enum.reduce(state.transports, [], fn(transport, transports) ->
      filter_transports(transports, write(transport, message(id, item)))
    end)
    {:noreply, %TransistorState{state | transports: transports}}
  end

  @doc """
  Add a transport to the transports's pool.
  """
  def handle_call({:add, transport, last_id}, _from, state) do
    transport = write_from_cache(transport, state.cache, last_id)
    {:reply, transport, %TransistorState{state | transports: [transport | state.transports]}}
  end

  defp filter_transports(transports, {:ok, transport}) do
    [ transport | transports ]
  end

  defp filter_transports(transports, {:error, _}) do
    transports
  end

  defp extract_name(token) do
    token |> String.to_atom
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
