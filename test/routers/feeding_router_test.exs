defmodule FeedingRouterTest do
  use Funnel.TestCase, async: true
  use Dynamo.HTTP.Case

  @endpoint FeedingRouter

  test "204 and an empty body" do
    conn = post("/", '{"fake":"body"}')
    assert conn.status == 204
    assert conn.resp_body == ""
  end
end