defmodule Funnel.Query do
  @doc """
  Create a query against an Elasticsearch index.

  * `index_id` - Index's id
  * `token`    - User's token
  * `query`    - Query in json
  """
  def create(index_id, token, query) do
    {status_code, body} = Funnel.Es.register(index_id, token, query)
    {:ok, status_code, body}
  end

  @doc """
  Update a query, or create a query with a specific id.

  * `index_id` - Index's id
  * `token`    - User's token
  * `uuid`     - Query's id
  * `query`    - Query in json
  """
  def update(index_id, token, uuid, query) do
    {status_code, body} = Funnel.Es.register(index_id, token, uuid, query)
    {:ok, status_code, body}
  end

  @doc """
  Remove a given query from a given index.
  * `index_id` - Index's id
  * `token`    - User's token
  * `uuid`     - Query's id
  """
  def destroy(index_id, token, uuid) do
    {status_code, body} = Funnel.Es.unregister(index_id, token, uuid)
    {:ok, status_code, body}
  end

  @doc """

  Returns a list of query for a given token.

  * `token`        - User's token. Mandatory.
  * `search_query` - Query as `Map`
  """
  def find(token, search_query \\ Map.new) do
    {status_code, body} = Funnel.Es.find(token, search_query)
    {:ok, status_code, body}
  end
end
