defmodule Funnel.QuerySearch do
  import Funnel.Es, only: [namespace: 1]
  import JSEX, only: [decode: 1, encode: 1]

  def query(token, search_query) do
    query_id = Dict.get(search_query, :query_id, "*")
    index_id = Dict.get(search_query, :index_id, "*")
    from = Dict.get(search_query, :from, 0)
    size = Dict.get(search_query, :size, 50)
    '{"query": {"bool": {"must": [{"query_string": {"default_field": "_id","query": "#{token}-#{query_id}"}},{"query_string":{"default_field": "_type","query": "#{namespace(index_id)}"}}]}},"from": #{from},"size": #{size}}'

  end

  def response(http) do
    {:ok, body} = decode http.body
    {:ok, body} = Enum.reduce(body["hits"]["hits"], [], &collect_query/2)
      |> encode
    {http.status_code, body}
  end

  defp collect_query(query, acc) do
    [serialize_query(query) | acc]
  end

  defp serialize_query(query) do
    [
      query_id: extract_query_id(query),
      index_id: extract_index_id(query),
      score: query["_score"]
    ]
  end

  defp extract_index_id(query) do
    String.split(query["_type"], "_")
      |> List.first
  end

  defp extract_query_id(query) do
    String.split(query["_id"], "-")
      |> List.last
  end
end
