defmodule Funnel.Query do
  import Funnel.Responder
  require Logger

  @doc """
  Create a query against an Elasticsearch index.

  * `index_id` - Index's id
  * `token`    - User's token
  * `query`    - Query in json
  """
  def create(index_id, token, query) do
    Logger.debug "[Query][Index: #{index_id}]: Create #{query}"
    Funnel.Es.register(index_id, token, query) |> respond
  end

  @doc """
  Update a query, or create a query with a specific id.

  * `index_id` - Index's id
  * `token`    - User's token
  * `uuid`     - Query's id
  * `query`    - Query in json
  """
  def update(index_id, token, uuid, query) do
    Logger.debug "[Query][Index: #{index_id}]: Update #{uuid} with #{query}"
    Funnel.Es.register(index_id, token, uuid, query) |> respond
  end

  @doc """
  Remove a given query from a given index.
  * `index_id` - Index's id
  * `token`    - User's token
  * `uuid`     - Query's id
  """
  def destroy(index_id, token, uuid) do
    Logger.debug "[Query][Index: #{index_id}]: Destroy #{uuid}"
    Funnel.Es.unregister(index_id, token, uuid) |> respond
  end

  @doc """

  Returns a list of query for a given token.

  * `token`        - User's token. Mandatory.
  * `search_query` - Query as `Map`
  """
  def find(token, search_query \\ Map.new) do
    Funnel.Es.find(token, search_query) |> respond
  end
end
