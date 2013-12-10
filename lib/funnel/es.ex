defmodule Funnel.Es do
  @module_doc """

  Interface to Elasticsearch
  """

  use HTTPotion.Base

  def process_url(url) do
    "http://localhost:9200" <> url
  end

  def percolate(body) do
    do_percolate(Funnel.Uuid.generate, body)
  end

  def percolate(uuid, body) do
    do_percolate(uuid, body)
  end

  def unpercolate do
    delete("/_percolator/funnel")
  end

  def unpercolate(id) do
    delete("/_percolator/funnel/#{id}")
  end

  defp do_percolate(uuid, body) do
    percolation = put("/_percolator/funnel/#{uuid}", body)
    {:ok, body} = JSEX.decode(percolation.body)
    {:ok, response} = JSEX.encode([filter_id: uuid, body: body])
    {percolation.status_code, response}
  end
end
