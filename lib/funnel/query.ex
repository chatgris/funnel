defmodule Funnel.Query do
  @doc """
  Create a query against an Elasticsearch index.

  * `index_id` - Index's id
  * `token`    - User's token
  * `query`    - Query in json
  """
  def create(index_id, token, query) do
    Funnel.Es.register(index_id, token, query)
  end

  @doc """
  Update a query, or create a query with a specific id.

  * `index_id` - Index's id
  * `token`    - User's token
  * `uuid`     - Query's id
  * `query`    - Query in json
  """
  def update(index_id, token, uuid, query) do
    Funnel.Es.register(index_id, token, uuid, query)
  end

  @doc """
  Remove a given query from a given index.
  * `index_id` - Index's id
  * `token`    - User's token
  * `uuid`     - Query's id
  """
  def destroy(index_id, token, uuid) do
    Funnel.Es.unregister(index_id, token, uuid)
  end

  @doc """

  Returns a list of query for a given token.

  * `token`        - User's token. Mandatory.
  * `search_query` - Query as `HashDict`
  """
  def find(token, search_query \\ HashDict.new) do
    Funnel.Es.find(token, search_query)
  end
end
