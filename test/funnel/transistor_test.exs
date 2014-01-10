defmodule Funnel.TransistorTest do
  use Funnel.TestCase, async: true
  use Dynamo.HTTP.Case

  test "transistor is alive" do
    conn = conn(:GET, "/?token=secrettoken")
    conn = conn.fetch(:params)
    {:ok, transistor} = Funnel.Transistor.start_link(conn)
    assert Process.alive?(transistor)
  end
end
