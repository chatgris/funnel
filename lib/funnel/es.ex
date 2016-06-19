defmodule Funnel.Es do
  @moduledoc """

  Interface to Elasticsearch:

  * create/delete indexes
  * register/unregister queries
  * percolate
  """

  use HTTPoison.Base
  alias Funnel.QuerySearch

  @doc """
  Returns the url used in the current request.

  This is the function used by HTTPotion to make everything works.
  """
  def process_url(url) do
    "#{host}#{url}"
  end

  @doc """
  Returns a list of query_id matched by the Elasticsearch percolation.
  """
  def percolate(index_id, body) do
    {:ok, body} = format_document(body)
    {:ok, percolation} = post("/#{namespace(index_id)}/messages/_percolate", body)
    {:ok, body} = Poison.decode percolation.body
    body["matches"] || []
  end

  @doc """
  Register a query against an Elasticsearch index.

  * `index_id` - Index's id
  * `token`    - User's token
  * `query`    - Query in json
  """
  def register(index_id, token, query) do
    register(index_id, token, Funnel.Uuid.generate, query)
  end

  @doc """
  Update a query, or create a query with a specific id.

  * `index_id` - Index's id
  * `token`    - User's token
  * `uuid`     - Query's id
  * `query`    - Query in json
  """
  def register(index_id, token, uuid, query) do
    id = "#{token}-#{uuid}"
    {:ok, percolation} = put("/#{index_id}_#{Mix.env}/.percolator/#{id}", query)
    {:ok, body} = Poison.decode(percolation.body)
    {:ok, response} = Poison.encode(%{query_id: uuid, index_id: index_id, body: body})
    {percolation.status_code, response}
  end

  @doc """
  Remove all queries from a given index.

  * `index_id` - Index's id
  """
  def unregister(index_id) do
    do_unregister("/#{namespace(index_id)}/.percolator/")
  end

  @doc """
  Remove a given query from a given index.
  * `index_id` - Index's id
  * `token`    - User's token
  * `uuid`     - Query's id
  """
  def unregister(index_id, token, id) do
    do_unregister("/#{namespace(index_id)}/.percolator/#{token}-#{id}")
  end

  @doc """

  Returns a list of query for a given token.

  * `token`        - User's token. Mandatory.
  * `search_query` - Query as `Map`
  """
  def find(token, search_query \\ Map.new) do
    index_id = search_query[:index_id] || "*"
    post("/#{namespace(index_id)}/.percolator/_search", QuerySearch.query(token, search_query))
      |> QuerySearch.response
  end

  @doc """
  Refresh Elasticsearch indexes.
  """
  def refresh do
    post("/_refresh", "")
  end

  @doc """
  Create an empty index.
  """
  def create do
    {:ok, response} = put("/#{namespace}", "")
    uuid = Funnel.Uuid.generate
    {:ok, body} = Poison.decode(response.body)
    {:ok, serialization} = Poison.encode(%{index_id: uuid, body: body})
    {response.status_code, serialization}
  end

  @doc """
  Create an empty index.
  """
  def create("") do
    create
  end

  @doc """
  Create an index with mappings and settings.

  * `body`     - Mappings and settings in json
  """
  def create(body) do
    uuid = Funnel.Uuid.generate
    {:ok, response} = post("/#{namespace(uuid)}", body)
    {:ok, body} = Poison.decode(response.body)
    {:ok, serialization} = Poison.encode(%{index_id: uuid, body: body})
    {response.status_code, serialization}
  end

  @doc """
  Create an index with mappings and settings.

  * `body`     - Mappings and settings in json
  """
  def create(body, uuid) do
    {:ok, response} = post("/#{namespace(uuid)}", body)
    {:ok, body} = Poison.decode(response.body)
    {:ok, serialization} = Poison.encode(%{index_id: uuid, body: body})
    {response.status_code, serialization}
  end

  @doc """
  Delete an index.
  """
  def destroy(index_id) do
    {:ok, response} = "/#{namespace(index_id)}"
      |> delete
    {response.status_code, response.body}
  end

  defp do_unregister(path) do
    {:ok, del} = delete path
    {del.status_code, del.body}
  end

  def namespace do
    "funnel_#{Mix.env}"
  end

  def namespace(index_id) do
    "#{index_id}_#{Mix.env}"
  end

  defp host do
    Application.get_env(:funnel, :es_host)
  end

  defp format_document(document) do
    {:ok, document} = Poison.decode(document)
    %{doc: document}
      |> Poison.encode
  end
end
