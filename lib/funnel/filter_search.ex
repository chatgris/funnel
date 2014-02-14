defmodule Funnel.FilterSearch do
  import Funnel.Es, only: [namespace: 1]
  import JSEX, only: [decode: 1, encode: 1]

  def query(token, search_filter) do
    filter_id = Dict.get(search_filter, :filter_id, "*")
    index_id = Dict.get(search_filter, :index_id, "*")
    from = Dict.get(search_filter, :from, 0)
    size = Dict.get(search_filter, :size, 50)
    '{"query": {"bool": {"must": [{"query_string": {"default_field": "_id","query": "#{token}-#{filter_id}"}},{"query_string":{"default_field": "_type","query": "#{namespace(index_id)}"}}]}},"from": #{from},"size": #{size}}'

  end

  def response(http) do
    {:ok, body} = decode http.body
    {:ok, body} = Enum.reduce(body["hits"]["hits"], [], &collect_filter/2)
      |> encode
    {http.status_code, body}
  end

  defp collect_filter(filter, acc) do
    [serialize_filter(filter) | acc]
  end

  defp serialize_filter(filter) do
    [
      filter_id: extract_filter_id(filter),
      index_id: extract_index_id(filter),
      score: filter["_score"]
    ]
  end

  defp extract_index_id(filter) do
    String.split(filter["_type"], "_")
      |> List.first
  end

  defp extract_filter_id(filter) do
    String.split(filter["_id"], "-")
      |> List.last
  end
end
