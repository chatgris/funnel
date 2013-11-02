defmodule Funnel.Es do
  @module_doc """

  Interface to Elasticsearch
  """

  use HTTPotion.Base

  def process_url(url) do
    "http://localhost:9200" <> url
  end
end
