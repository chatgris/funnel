defmodule Funnel do
  use Application

  @doc """
  The application callback used to start this
  application and its Dynamos.
  """
  def start(_type, _args) do
    Funnel.Supervisor.start_link
  end

  @doc """
  Register a new transport.

  * `transport`    - Something implementing the `Funnel.Transport` protocol
  * `last_id`      - the last id received in the transport
  """
  def register(transport, last_id \\ nil) do
    token = Funnel.Uuid.generate
    register(transport, token, last_id)
    {:ok, token}
  end

  @doc """
  Register a new transport.

  * `transport`    - Something implementing the `Funnel.Transport` protocol
  * `token`        - User's token
  * `last_id`      - the last id received in the transport
  """
  def register(transport, token, last_id) do
    {:ok, _transistor} = Funnel.Transistors.add token
    Funnel.Transistor.add(transport, token, last_id)
    {:ok, token}
  end

  @doc """
  Send a document to Elasticsearch's percolator.

  * `index_id` - the index_id use for this perticular document
  * `body`     - Document in json
  """
  def percolate(index_id, body) do
    {Funnel.PercolatorPool.percolate(index_id, body)}
  end
end
