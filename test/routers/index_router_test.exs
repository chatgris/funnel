defmodule IndexRouterTest do
  use Funnel.TestCase
  use Dynamo.HTTP.Case

  @endpoint IndexRouter

  test "create an index" do
    body = '{"settings" : {"number_of_shards" : 1},"mappings" : {"type1" : {"_source" : { "enabled" : false },"properties" : {"field1" : { "type" : "string", "index" : "not_analyzed" }}}}}'
    conn = conn(:POST, "/?token=token")
    conn = conn.put_req_header "Content-Type", "application/json"
    conn = post(conn, "/?token=token", body)
    {:ok, body} = JSEX.decode conn.resp_body
    uuid = body["index_id"]
    assert conn.status == 200
    assert conn.resp_headers["Content-Type"] == "application/json"
    Funnel.Es.destroy(uuid)
  end
end
