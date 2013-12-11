defmodule FeedingRouterTest do
  use Funnel.TestCase, async: true
  use Dynamo.HTTP.Case

  @endpoint FeedingRouter

  test "204 and an empty body" do
    conn = post("/", "fake_body")
    assert conn.status == 204
    assert conn.resp_body == nil
  end
end
