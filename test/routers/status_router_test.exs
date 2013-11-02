defmodule StatusRouterTest do
  use Funnel.TestCase
  use Dynamo.HTTP.Case

  @endpoint StatusRouter

  test "returns 200" do
    conn = get("/")
    assert conn.status == 200
  end

  test "returns json" do
    conn = get("/")
    assert conn.resp_headers["Content-Type"] == "application/json"
  end

  test "returns full response from ES" do
    conn = get("/")
    {:ok, payload} = JSEX.decode(conn.resp_body)
    assert payload["status"] == 200
  end
end
