defmodule OhaiRouterTest do
  use Funnel.TestCase, async: true
  use Dynamo.HTTP.Case

  @endpoint OhaiRouter

  test "returns 201" do
    conn = post("/")
    assert conn.status == 201
  end

  test "returns json" do
    conn = post("/")
    assert conn.resp_headers["Content-Type"] == "application/json"
  end

  test "returns a token" do
    conn = post("/")
    {:ok, body} = JSEX.decode conn.resp_body
    assert size(body["token"]) == 32
  end

end
