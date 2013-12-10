defmodule Funnel.Es do
  @module_doc """

  Interface to Elasticsearch
  """

  use HTTPotion.Base

  def process_url(url) do
    "http://localhost:9200" <> url
  end

  def percolate(body) do
    uuid = Funnel.Uuid.generate
    percolation = put("/_percolator/funnel/#{uuid}", body)
    {:ok, body} = JSEX.decode(percolation.body)
    {:ok, response} = JSEX.encode([query_id: uuid, body: body])
    {percolation.status_code, response}
  end

  def unpercolate do
    delete("/_percolator/funnel")
  end

  def unpercolate(id) do
    delete("/_percolator/funnel/#{id}")
  end
end
