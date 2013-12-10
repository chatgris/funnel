# Feature tests go through the Dynamo.under_test
# and are meant to test the full stack.
defmodule HomeTest do
  use Funnel.TestCase, async: true
  use Dynamo.HTTP.Case

  test "returns OK" do
    conn = get("/")
    assert conn.status == 200
  end

  test "returns json" do
    conn = get("/")
    assert conn.resp_headers["Content-Type"] == "application/json"
  end
end
