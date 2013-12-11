defmodule Funnel.Es do
  @module_doc """

  Interface to Elasticsearch
  """

  use HTTPotion.Base

  def process_url(url) do
    "http://localhost:9200" <> url
  end

  # Noop atm
  def percolate(body) do
    body
  end

  def register(body) do
    do_percolate(Funnel.Uuid.generate, body)
  end

  def register(uuid, body) do
    do_percolate(uuid, body)
  end

  def unregister do
    do_unpercolate("/_percolator/funnel")
  end

  def unregister(id) do
    do_unpercolate("/_percolator/funnel_#{Mix.env}/#{id}")
  end

  defp do_unpercolate(path) do
    del = delete path
    {del.status_code, del.body}
  end

  defp do_percolate(uuid, body) do
    percolation = put("/_percolator/funnel_#{Mix.env}/#{uuid}", body)
    {:ok, body} = JSEX.decode(percolation.body)
    {:ok, response} = JSEX.encode([filter_id: uuid, body: body])
    {percolation.status_code, response}
  end
end
