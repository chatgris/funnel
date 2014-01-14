defmodule Funnel.Es do
  @moduledoc """

  Interface to Elasticsearch:

  * create/delete indexes
  * register/unregister filters
  * percolate
  """

  use HTTPotion.Base

  @doc """

  Returns the url used in the current request.

  This is the function used by HTTPotion to make everything works.
  """
  def process_url(url) do
    "#{host}#{url}"
  end

  @doc """

  Returns a list of filter_id matched by the Elasticsearch percolation.
  """
  def percolate(body) do
    percolation = post("/#{namespace}/message/_percolate", body)
    {:ok, body} = JSEX.decode percolation.body
    body["matches"] || []
  end

  @doc """

  Register a filter against an Elasticsearch index.

  * `index_id` - Index's id
  * `token`    - User's token
  * `body`     - Filter in json
  """
  def register(index_id, token, body) do
    do_percolate(index_id, token, Funnel.Uuid.generate, body)
  end

  @doc """

  Update a filter, or create a filter with a specific id.

  * `index_id` - Index's id
  * `token`    - User's token
  * `uuid`     - Filter's id
  * `body`     - Filter in json
  """
  def register(index_id, token, uuid, body) do
    do_percolate(index_id, token, uuid, body)
  end

  @doc """

  Remove all filters from a given index.

  * `index_id` - Index's id
  """
  def unregister(index_id) do
    do_unpercolate("/_percolator/#{namespace(index_id)}")
  end

  @doc """

  Remove a given filter from a given index.

  * `index_id` - Index's id
  * `token`    - User's token
  * `uuid`     - Filter's id
  """
  def unregister(index_id, token, id) do
    do_unpercolate("/_percolator/#{namespace(index_id)}/#{token}-#{id}")
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
    put("/#{namespace}", "")
  end

  @doc """

  Create an index with mappings and settings.

  * `body`     - Mappings and settings in json
  """
  def create(body) do
    uuid = Funnel.Uuid.generate
    response = post(namespace(uuid), body)
    {:ok, body} = JSEX.decode(response.body)
    {:ok, serialization} = JSEX.encode([index_id: uuid, body: body])
    {response.status_code, serialization}
  end

  @doc """

  Delete an index.
  """
  def destroy(index_id) do
    namespace(index_id)
      |> delete
  end

  defp do_unpercolate(path) do
    del = delete path
    {del.status_code, del.body}
  end

  defp do_percolate(index_id, token, uuid, body) do
    id = "#{token}-#{uuid}"
    percolation = put("/_percolator/#{index_id}_#{Mix.env}/#{id}", body)
    {:ok, body} = JSEX.decode(percolation.body)
    {:ok, response} = JSEX.encode([filter_id: uuid, body: body])
    {percolation.status_code, response}
  end

  defp namespace do
    "funnel_#{Mix.env}"
  end

  defp namespace(index_id) do
    "/#{index_id}_#{Mix.env}"
  end

  defp host do
    :os.getenv("ES_HOST") || "http://localhost:9200"
  end
end
