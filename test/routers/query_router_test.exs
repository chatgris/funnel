defmodule QueryRouterTest do
  use Funnel.TestCase
  use Dynamo.HTTP.Case

  @endpoint QueryRouter

  test "returns 400 when an empty body is given" do
    conn = post("/?token=token&index_id=funnel")
    assert conn.status == 400
  end

  test "returns 400 when an empty body is given on put" do
    conn = put("/uuid?token=token&index_id=funnel")
    assert conn.status == 400
  end

  test "create a query" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    conn = post("/?token=token&index_id=funnel", body)
    conn = conn.put_req_header "Content-Type", "application/json"
    {:ok, body} = JSEX.decode conn.resp_body
    uuid = body["query_id"]
    assert conn.status == 201
    assert conn.resp_headers["Content-Type"] == "application/json"
    Funnel.Es.unregister("funnel", "token", uuid)
  end

  test "update a query" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    conn = post("/?token=token&index_id=funnel", body)
    conn = conn.put_req_header "Content-Type", "application/json"
    assert conn.status == 201
    {:ok, body} = JSEX.decode conn.resp_body
    uuid = body["query_id"]
    body = '{"query" : {"term" : {"field1" : "value2"}}}'
    conn = put("#{uuid}?token=token&index_id=funnel", body)
    conn = conn.put_req_header "Content-Type", "application/json"
    assert conn.status == 200
    Funnel.Es.unregister("funnel", "token", uuid)
  end

  test "delete a existing query" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    conn = post("/?token=token&index_id=funnel", body)
    conn = conn.put_req_header "Content-Type", "application/json"
    assert conn.status == 201
    {:ok, body} = JSEX.decode conn.resp_body
    uuid = body["query_id"]
    conn = conn(:PUT, uuid)
    conn = conn.put_req_header "Content-Type", "application/json"
    conn = delete(conn, "#{uuid}?token=token&index_id=funnel")
    assert conn.status == 200
    Funnel.Es.unregister("funnel", "token", uuid)
  end

  test "delete a non-existing query" do
    uuid = "uuid"
    conn = conn(:PUT, uuid)
    conn = conn.put_req_header "Content-Type", "application/json"
    conn = delete(conn, "#{uuid}?token=token&index_id=funnel")
    assert conn.status == 404
  end

  test "returns a query_id" do
    body = '{"query" : {"term" : {"field1" : "value1"}}}'
    conn = post("/?token=token&index_id=funnel", body)
    conn = conn.put_req_header "Content-Type", "application/json"
    {:ok, body} = JSEX.decode conn.resp_body
    uuid = body["query_id"]
    assert size(uuid) == 32
    Funnel.Es.unregister("funnel", "token", uuid)
  end
end
