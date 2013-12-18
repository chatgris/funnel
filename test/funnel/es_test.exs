defmodule EsTest do
  use Funnel.TestCase, async: true

  Funnel.Es.create

  test "returns a 201 on query creation" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    {status, response} = Funnel.Es.register(body)
    {:ok, body} = JSEX.decode response
    assert status == 201
    Funnel.Es.unregister(body["filter_id"])
  end

  test "update a query creation" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    {status, response} = Funnel.Es.register(body)
    {:ok, body} = JSEX.decode response
    uuid = body["filter_id"]
    assert status == 201

    body = '{"query" : {"term" : {"field1" : "value2"}}}'
    {status, response} = Funnel.Es.register(uuid, body)
    {:ok, body} = JSEX.decode response
    assert status == 200
    Funnel.Es.unregister(body["filter_id"])
  end

  test "returns a body on query creation" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    {_, response} = Funnel.Es.register(body)
    {:ok, body} = JSEX.decode response
    assert size(body["filter_id"]) == 32
    Funnel.Es.unregister(body["filter_id"])
  end

  test "returns a 400 on bad payload" do
    {status, response} = Funnel.Es.register("")
    {:ok, body} = JSEX.decode response
    assert status == 400
    Funnel.Es.unregister(body["filter_id"])
    Funnel.Es.refresh
  end

  test "returns queries from percolator" do
    body = '{"query" : {"field" : {"message" : "elasticsearch"}}}'
    message = '{"doc" : {"message" : "this new elasticsearch percolator feature is nice, borat style"}}'
    {_, response} = Funnel.Es.register(body)
    {:ok, body} = JSEX.decode response
    uuid = body["filter_id"]
    Funnel.Es.refresh
    assert Funnel.Es.percolate(message) == [uuid]
    Funnel.Es.unregister(uuid)
  end

  test "returns empty from percolator on non match" do
    message = '{"doc" : {"message" : "Ohai"}}'
    assert Funnel.Es.percolate(message) == []
  end
end
