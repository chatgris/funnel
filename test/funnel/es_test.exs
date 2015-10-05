defmodule EsTest do
  use Funnel.TestCase

  Funnel.Es.create
  import Funnel.Es.Asserts

  test "returns a 201 on query creation" do
    body = assert_query_creation('{"query" : {"term" : {"field1" : "value1"}}}', "funnel")
    Funnel.Es.unregister("funnel", "token", body["query_id"])
  end

  test "update a query creation" do
    uuid = assert_query_creation('{"query" : {"term" : {"field1" : "value1"}}}', "funnel")["query_id"]
    assert_query_update('{"query" : {"term" : {"field1" : "value2"}}}', uuid)
    Funnel.Es.unregister("funnel", "token", uuid)
  end

  test "returns a body on query creation" do
    body = assert_query_creation('{"query" : {"term" : {"field1" : "value1"}}}', "funnel")
    assert byte_size(body["query_id"]) == 32
    assert body["index_id"] == "funnel"
    Funnel.Es.unregister("funnel", "token", body["query_id"])
  end

  test "returns a 400 on bad payload" do
    {status, _} = Funnel.Es.register("funnel", "token", "")
    assert status == 400
  end

  test "returns queries from percolator" do
    uuid = assert_query_creation('{"query" : {"match" : {"message" : "elasticsearch"}}}')["query_id"]
    message = '{"message" : "this new elasticsearch percolator feature is nice, borat style"}' |> IO.iodata_to_binary
    Funnel.Es.refresh
    Funnel.Es.percolate("funnel", message)
      |> Enum.each(fn(match) -> assert_percolate(match, uuid) end)
    Funnel.Es.unregister("funnel", "token", uuid)
  end

  test "returns empty from percolator on non match" do
    message = '{"message" : "Ohai"}' |> IO.iodata_to_binary
    assert Funnel.Es.percolate("funnel", message) == []
  end

  test "creates a new index" do
    body = '{"settings" : {"number_of_shards" : 1},"mappings" : {"type1" : {"_source" : { "enabled" : false },"properties" : {"field1" : { "type" : "string", "index" : "not_analyzed" }}}}}'
    {_, response} = Funnel.Es.create(body)
    {:ok, body} = Poison.decode response
    uuid = body["index_id"]
    assert byte_size(uuid) == 32
    Funnel.Es.destroy(uuid)
  end

  test "find a query based on token" do
    uuid = assert_query_creation('{"query" : {"term" : {"field1" : "value1"}}}')["query_id"]
    Funnel.Es.refresh
    search_query = %{query_id: uuid}
    {status, response} = Funnel.Es.find("token", search_query)
    {:ok, response} = Poison.decode response
    assert status == 200
    assert Enum.count(response) == 1
    item = Enum.at(response, 0)
    assert item["query_id"] == uuid
    assert item["index_id"] == "funnel"
    Funnel.Es.unregister("funnel", "token", uuid)
  end

  test "find several queries based on token" do
    Funnel.Es.Asserts.create_index("multiple_index")
    Funnel.Es.refresh
    uuid = assert_query_creation('{"query" : {"term" : {"field1" : "value1"}}}', "multiple_index", "multiple_token")["query_id"]
    uuid2 = assert_query_creation('{"query" : {"term" : {"field1" : "value2"}}}', "multiple_index", "multiple_token")["query_id"]
    Funnel.Es.refresh
    response = assert_query_find("*", "multiple_token")
    assert Enum.count(response) == 2
    Funnel.Es.unregister("multiple_index", "multiple_token", uuid)
    Funnel.Es.unregister("multiple_index", "multiple_token", uuid2)
  end

  test "find several queries based on different index" do
    uuid = assert_query_creation('{"query" : {"term" : {"field1" : "value1"}}}', "index1")["query_id"]
    uuid2 = assert_query_creation('{"query" : {"term" : {"field1" : "value2"}}}', "index2")["query_id"]
    Funnel.Es.refresh
    response = assert_query_find("index1")
    assert Enum.count(response) == 1
    response = assert_query_find("index2")
    assert Enum.count(response) == 1
    Funnel.Es.unregister("index1", "token", uuid)
    Funnel.Es.unregister("index2", "token", uuid2)
  end
end
