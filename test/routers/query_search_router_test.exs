defmodule QuerySearchRouterTest do
  use Funnel.TestCase
  use Dynamo.HTTP.Case

  @endpoint QuerySearchRouter

  test "search queries based on index_id" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    {status, response} = Funnel.Es.register("funnel", "tokenrouter", body)
    {:ok, body} = JSEX.decode response
    uuid = body["query_id"]
    assert status == 201
    Funnel.Es.refresh
    conn = get("/?token=tokenrouter&index_id=funnel")
    conn = conn.put_req_header "Content-Type", "application/json"
    {:ok, body} = JSEX.decode conn.resp_body
    assert Enum.count(body) == 1
    Funnel.Es.unregister("funnel", "tokenrouter", uuid)
  end

  test "search queries based on token" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    {status, response} = Funnel.Es.register("funnel", "tokenroutersearch", body)
    {:ok, body} = JSEX.decode response
    uuid = body["query_id"]
    assert status == 201
    Funnel.Es.refresh
    conn = get("/?token=tokenroutersearch")
    conn = conn.put_req_header "Content-Type", "application/json"
    {:ok, body} = JSEX.decode conn.resp_body
    assert Enum.count(body) == 1
    Funnel.Es.unregister("funnel", "tokenroutersearch", uuid)
  end
end
