defmodule FunnelTest do
  use Funnel.TestCase

  def create_index do
    settings = '{"settings" : {"number_of_shards" : 1},"mappings" : {"type1" : {"_source" : { "enabled" : false },"properties" : {"field1" : { "type" : "string", "index" : "not_analyzed" }}}}}' |> IO.iodata_to_binary
    Funnel.Index.create(settings)
  end

  def assert_create_query do
    {:ok, _status_code, body} = create_index
    index_id = body["index_id"]
    query = '{"query" : {"match" : {"message" : "elasticsearch"}}}' |> IO.iodata_to_binary

    {:ok, status_code, body} = Funnel.Query.create(index_id, "token", query)
    query_id = body["query_id"]
    assert status_code == 201
    assert body != nil
    {index_id, query_id}
  end

  test "register a transport" do
    {:ok, token} = Funnel.register(self)
    assert token != nil
  end

  test "create an empty index" do
    {:ok, _status_code, body} = Funnel.Index.create
    index_id = body["index_id"]
    assert size(index_id) == 32
    Funnel.Es.destroy(index_id)
  end

  test "create an index with settings" do
    {:ok, _status_code, body} = create_index
    index_id = body["index_id"]
    assert size(index_id) == 32
    Funnel.Es.destroy(index_id)
  end

  test "delete an index" do
    {:ok, _status_code, body} = create_index
    index_id = body["index_id"]
    Funnel.Es.refresh
    {:ok, status_code, body} = Funnel.Index.destroy(index_id)
    assert status_code == 200
    assert body != nil
  end

  test "create a query" do
    {index_id, query_id} = assert_create_query
    Funnel.Query.destroy(index_id, "token", query_id)
    Funnel.Es.destroy(index_id)
  end

  test "update a query" do
    query = '{"query" : {"match" : {"message" : "funnel"}}}' |> IO.iodata_to_binary
    {index_id, query_id} = assert_create_query
    {:ok, status_code, body} = Funnel.Query.update(index_id, "token", query_id, query)
    assert status_code == 200
    assert body != nil
    Funnel.Query.destroy(index_id, "token", query_id)
    Funnel.Es.destroy(index_id)
  end

  test "destroy a query" do
    {index_id, query_id} = assert_create_query
    {:ok, status_code, body} = Funnel.Query.destroy(index_id, "token", query_id)
    assert status_code == 200
    assert body != nil
    Funnel.Es.destroy(index_id)
  end

  test "find a query from a token" do
    {index_id, query_id} = assert_create_query
    Funnel.Es.refresh
    {:ok, status_code, body} = Funnel.Query.find("token")
    assert status_code == 200
    assert Enum.count(body) == 1
    Funnel.Query.destroy(index_id, "token", query_id)
    Funnel.Es.destroy(index_id)
  end

  test "find a query from a token and index_id" do
    {index_id, query_id} = assert_create_query
    Funnel.Es.refresh
    {:ok, status_code, body} = Funnel.Query.find("token", %{index_id: index_id})
    assert status_code == 200
    assert Enum.count(body) == 1
    Funnel.Query.destroy(index_id, "token", query_id)
    Funnel.Es.destroy(index_id)
  end

  test "submit a message to the percolator" do
    message = '{"message" : "this new elasticsearch percolator feature is nice, borat style"}' |> IO.iodata_to_binary
    assert Funnel.percolate("funnel", message) == {:ok}
  end
end
