defmodule EsTest do
  use Funnel.TestCase

  Funnel.Es.create

  def assert_query_creation(query, index \\ "funnel") do
    {status, response} = Funnel.Es.register(index, "token", query)
    {:ok, body} = JSEX.decode response
    assert status == 201
    body
  end

  def assert_query_update(query, uuid, index \\ "funnel") do
    {status, response} = Funnel.Es.register(index, "token", uuid, query)
    {:ok, body} = JSEX.decode response
    assert status == 200
    body
  end

  test "returns a 201 on query creation" do
    body = assert_query_creation('{"query" : {"term" : {"field1" : "value1"}}}')
    Funnel.Es.unregister("funnel", "token", body["query_id"])
  end

  test "update a query creation" do
    uuid = assert_query_creation('{"query" : {"term" : {"field1" : "value1"}}}')["query_id"]
    assert_query_update('{"query" : {"term" : {"field1" : "value2"}}}', uuid)
    Funnel.Es.unregister("funnel", "token", uuid)
  end

  test "returns a body on query creation" do
    body = assert_query_creation('{"query" : {"term" : {"field1" : "value1"}}}')
    assert size(body["query_id"]) == 32
    assert body["index_id"] == "funnel"
    Funnel.Es.unregister("funnel", "token", body["query_id"])
  end

  test "returns a 400 on bad payload" do
    {status, _} = Funnel.Es.register("funnel", "token", "")
    assert status == 400
  end

  test "returns queries from percolator" do
    uuid = assert_query_creation('{"query" : {"field" : {"message" : "elasticsearch"}}}')["query_id"]
    message = '{"doc" : {"message" : "this new elasticsearch percolator feature is nice, borat style"}}'
    Funnel.Es.refresh
    assert Funnel.Es.percolate("funnel", message) == ["token-#{uuid}"]
    Funnel.Es.unregister("funnel", "token", uuid)
  end

  test "returns empty from percolator on non match" do
    message = '{"doc" : {"message" : "Ohai"}}'
    assert Funnel.Es.percolate("funnel", message) == []
  end

  test "creates a new index" do
    body = '{"settings" : {"number_of_shards" : 1},"mappings" : {"type1" : {"_source" : { "enabled" : false },"properties" : {"field1" : { "type" : "string", "index" : "not_analyzed" }}}}}'
    {_, response} = Funnel.Es.create(body)
    {:ok, body} = JSEX.decode response
    uuid = body["index_id"]
    assert size(uuid) == 32
    Funnel.Es.destroy(uuid)
  end

  test "find a query based on token" do
    uuid = assert_query_creation('{"query" : {"term" : {"field1" : "value1"}}}')["query_id"]
    Funnel.Es.refresh
    hash = HashDict.new
    hash = Dict.put(hash, :query_id, uuid)
    {status, response} = Funnel.Es.find("token", hash)
    {:ok, response} = JSEX.decode response
    assert status == 200
    assert Enum.count(response) == 1
    item = Enum.at(response, 0)
    assert item["query_id"] == uuid
    assert item["index_id"] == "funnel"
    Funnel.Es.unregister("funnel", "token", uuid)
  end

  test "find several queries based on token" do
    uuid = assert_query_creation('{"query" : {"term" : {"field1" : "value1"}}}')["query_id"]
    body = '{"query" : {"term" : {"field1" : "value2"}}}'
    {status, response} = Funnel.Es.register("funnel", "token", body)
    assert status == 201
    {:ok, body} = JSEX.decode response
    uuid2 = body["query_id"]
    Funnel.Es.refresh
    {status, response} = Funnel.Es.find("token")
    {:ok, response} = JSEX.decode response
    assert status == 200
    assert Enum.count(response) == 2
    Funnel.Es.unregister("funnel", "token", uuid)
    Funnel.Es.unregister("funnel", "token", uuid2)
  end

  test "find several queries based on different index" do
    uuid = assert_query_creation('{"query" : {"term" : {"field1" : "value1"}}}', "index1")["query_id"]
    uuid2 = assert_query_creation('{"query" : {"term" : {"field1" : "value2"}}}', "index2")["query_id"]
    Funnel.Es.refresh
    hash = HashDict.new
    hash = Dict.put(hash, :index_id, "index1")
    {status, response} = Funnel.Es.find("token", hash)
    {:ok, response} = JSEX.decode response
    assert status == 200
    assert Enum.count(response) == 1
    hash = HashDict.new
    hash = Dict.put(hash, :index_id, "index2")
    {status, response} = Funnel.Es.find("token", hash)
    {:ok, response} = JSEX.decode response
    assert status == 200
    assert Enum.count(response) == 1
    Funnel.Es.unregister("index1", "token", uuid)
    Funnel.Es.unregister("index2", "token", uuid2)
  end
end
