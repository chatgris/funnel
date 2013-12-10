defmodule EsTest do
  use Funnel.TestCase

  test "returns a 201 on query creation" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    {status, _} = Funnel.Es.percolate(body)
    assert status == 201
    Funnel.Es.unpercolate
  end

  test "returns a body on query creation" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    {_, response} = Funnel.Es.percolate(body)
    {:ok, body} = JSEX.decode response
    assert size(body["query_id"]) == 36
    Funnel.Es.unpercolate
  end

  test "returns a 400 on bad payload" do
    {status, _} = Funnel.Es.percolate("")
    assert status == 400
    Funnel.Es.unpercolate
  end
end
