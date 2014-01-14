defmodule Funnel.Es do
  @module_doc """

  Interface to Elasticsearch
  """

  use HTTPotion.Base

  def process_url(url) do
    "#{host}#{url}"
  end

  def host do
    :os.getenv("ES_HOST") || "http://localhost:9200"
  end

  # Noop atm
  def percolate(body) do
    percolation = post("/#{namespace}/message/_percolate", body)
    {:ok, body} = JSEX.decode percolation.body
    body["matches"] || []
  end

  def register(index_id, token, body) do
    do_percolate(index_id, token, Funnel.Uuid.generate, body)
  end

  def register(index_id, token, uuid, body) do
    do_percolate(index_id, token, uuid, body)
  end

  def unregister(index_id) do
    do_unpercolate("/_percolator/#{namespace(index_id)}")
  end

  def unregister(index_id, token, id) do
    do_unpercolate("/_percolator/#{namespace(index_id)}/#{token}-#{id}")
  end

  def refresh do
    post("/_refresh", "")
  end

  def create do
    put("/#{namespace}", "")
  end

  def create(body) do
    uuid = Funnel.Uuid.generate
    response = post(namespace(uuid), body)
    {:ok, body} = JSEX.decode(response.body)
    {:ok, serialization} = JSEX.encode([index_id: uuid, body: body])
    {response.status_code, serialization}
  end

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
end
