defmodule Funnel.TransistorTest do
  use Funnel.TestCase, async: true
  use Dynamo.HTTP.Case

  test "transistor is alive" do
    token = "secrettoken"
    conn = conn(:GET, "/?token=#{token}")
    conn.fetch(:params)
    {:ok, transistor} = Funnel.Transistor.start_link(token)
    assert Process.alive?(transistor)
  end
end
