defmodule FeedingRouterTest do
  use Funnel.TestCase, async: true
  use Dynamo.HTTP.Case

  @endpoint FeedingRouter

  test "204 and an empty body" do
    json = JSEX.encode [fake: "body"]
    conn = post("/", json)
    assert conn.status == 204
    assert conn.resp_body == ""
  end
end
