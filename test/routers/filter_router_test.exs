defmodule FilterRouterTest do
  use Funnel.TestCase
  use Dynamo.HTTP.Case

  @endpoint FilterRouter

  test "returns 400 when an empty body is given" do
    conn = post("/?token=token&index_id=funnel")
    assert conn.status == 400
  end

  test "returns 400 when an empty body is given on put" do
    conn = put("/uuid?token=token&index_id=funnel")
    assert conn.status == 400
  end

  test "create a filter" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    conn = post("/?token=token&index_id=funnel", body)
    conn = conn.put_req_header "Content-Type", "application/json"
    {:ok, body} = JSEX.decode conn.resp_body
    uuid = body["filter_id"]
    assert conn.status == 201
    assert conn.resp_headers["Content-Type"] == "application/json"
    Funnel.Es.unregister("funnel", "token", uuid)
  end

  test "update a filter" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    conn = post("/?token=token&index_id=funnel", body)
    conn = conn.put_req_header "Content-Type", "application/json"
    assert conn.status == 201
    {:ok, body} = JSEX.decode conn.resp_body
    uuid = body["filter_id"]
    body = '{"query" : {"term" : {"field1" : "value2"}}}'
    conn = put("#{uuid}?token=token&index_id=funnel", body)
    conn = conn.put_req_header "Content-Type", "application/json"
    assert conn.status == 200
    Funnel.Es.unregister("funnel", "token", uuid)
  end

  test "delete a existing filter" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    conn = post("/?token=token&index_id=funnel", body)
    conn = conn.put_req_header "Content-Type", "application/json"
    assert conn.status == 201
    {:ok, body} = JSEX.decode conn.resp_body
    uuid = body["filter_id"]
    conn = conn(:PUT, uuid)
    conn = conn.put_req_header "Content-Type", "application/json"
    conn = delete(conn, "#{uuid}?token=token&index_id=funnel")
    assert conn.status == 200
    Funnel.Es.unregister("funnel", "token", uuid)
  end

  test "delete a non-existing filter" do
    uuid = "uuid"
    conn = conn(:PUT, uuid)
    conn = conn.put_req_header "Content-Type", "application/json"
    conn = delete(conn, "#{uuid}?token=token&index_id=funnel")
    assert conn.status == 404
  end

  test "returns a filter_id" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    conn = post("/?token=token&index_id=funnel", body)
    conn = conn.put_req_header "Content-Type", "application/json"
    {:ok, body} = JSEX.decode conn.resp_body
    uuid = body["filter_id"]
    assert size(uuid) == 32
    Funnel.Es.unregister("funnel", "token", uuid)
  end

  test "search filters based on index_id" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    {status, response} = Funnel.Es.register("funnel", "tokenrouter", body)
    {:ok, body} = JSEX.decode response
    uuid = body["filter_id"]
    assert status == 201
    Funnel.Es.refresh
    conn = get("/?token=tokenrouter&index_id=funnel")
    conn = conn.put_req_header "Content-Type", "application/json"
    {:ok, body} = JSEX.decode conn.resp_body
    assert Enum.count(body) == 1
    Funnel.Es.unregister("funnel", "tokenrouter", uuid)
  end
end
