defmodule EsTest do
  use Funnel.TestCase, async: true

  Funnel.Es.create

  test "returns a 201 on query creation" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    {status, response} = Funnel.Es.register("funnel", "token", body)
    {:ok, body} = JSEX.decode response
    assert status == 201
    Funnel.Es.unregister("funnel", "token", body["filter_id"])
  end

  test "update a query creation" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    {status, response} = Funnel.Es.register("funnel", "token", body)
    {:ok, body} = JSEX.decode response
    uuid = body["filter_id"]
    assert status == 201

    body = '{"query" : {"term" : {"field1" : "value2"}}}'
    {status, response} = Funnel.Es.register("funnel", "token", uuid, body)
    {:ok, body} = JSEX.decode response
    assert status == 200
    Funnel.Es.unregister("funnel", "token", body["filter_id"])
  end

  test "returns a body on query creation" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    {_, response} = Funnel.Es.register("funnel", "token", body)
    {:ok, body} = JSEX.decode response
    assert size(body["filter_id"]) == 32
    Funnel.Es.unregister("funnel", "token", body["filter_id"])
  end

  test "returns a 400 on bad payload" do
    {status, _} = Funnel.Es.register("funnel", "token", "")
    assert status == 400
  end

  test "returns queries from percolator" do
    body = '{"query" : {"field" : {"message" : "elasticsearch"}}}'
    message = '{"doc" : {"message" : "this new elasticsearch percolator feature is nice, borat style"}}'
    {_, response} = Funnel.Es.register("funnel", "token", body)
    {:ok, body} = JSEX.decode response
    uuid = body["filter_id"]
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
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    {status, response} = Funnel.Es.register("funnel", "token", body)
    {:ok, body} = JSEX.decode response
    uuid = body["filter_id"]
    assert status == 201
    Funnel.Es.refresh
    {status, response} = Funnel.Es.find("token", uuid)
    assert status == 200
    assert Enum.count(response) == 1
    Funnel.Es.unregister("funnel", "token", uuid)
  end

  test "find several queries based on token" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    {status, response} = Funnel.Es.register("funnel", "token", body)
    {:ok, body} = JSEX.decode response
    uuid = body["filter_id"]
    assert status == 201
    body = '{"query" : {"term" : {"field1" : "value2"}}}'
    {status, response} = Funnel.Es.register("funnel", "token", body)
    assert status == 201
    {:ok, body} = JSEX.decode response
    uuid2 = body["filter_id"]
    Funnel.Es.refresh
    {status, response} = Funnel.Es.find("token")
    assert status == 200
    assert Enum.count(response) == 2
    Funnel.Es.unregister("funnel", "token", uuid)
    Funnel.Es.unregister("funnel", "token", uuid2)
  end

  test "find several queries based on different index" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    {status, response} = Funnel.Es.register("index1", "token", body)
    {:ok, body} = JSEX.decode response
    uuid = body["filter_id"]
    assert status == 201
    body = '{"query" : {"term" : {"field1" : "value2"}}}'
    {status, response} = Funnel.Es.register("index2", "token", body)
    assert status == 201
    {:ok, body} = JSEX.decode response
    uuid2 = body["filter_id"]
    Funnel.Es.refresh
    {status, response} = Funnel.Es.find("token")
    assert status == 200
    assert Enum.count(response) == 2
    {status, response} = Funnel.Es.find("token", "*", "index1")
    assert status == 200
    assert Enum.count(response) == 1
    {status, response} = Funnel.Es.find("token", "*", "index2")
    assert status == 200
    assert Enum.count(response) == 1
    Funnel.Es.unregister("index1", "token", uuid)
    Funnel.Es.unregister("index2", "token", uuid2)
  end
end
