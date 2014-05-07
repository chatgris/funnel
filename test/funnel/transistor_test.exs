defimpl Funnel.Transport, for: PID do
  def write(receiver, message) do
    send(receiver, {:chunk, message})
    {:ok, receiver}
  end
end

defimpl Funnel.Transport, for: Dynamo.Connection.Test do
  def write(conn, %{:id => _, :item => item}) do
    conn.chunk(item)
  end
end

defmodule Funnel.TransistorTest do
  use Funnel.TestCase, async: true
  use Dynamo.HTTP.Case

  test "transistor is alive" do
    token = "secrethttptoken"
    conn(:GET, "/?token=#{token}")
    {:ok, transistor} = Funnel.Transistor.start_link(token)
    assert Process.alive?(transistor)
  end

  test "receiver is a dynamo conn" do
    token = "secrethttptoken"
    conn = conn(:GET, "/?token=#{token}")
    conn = conn.send_chunked(200)
    {:ok, transistor} = Funnel.Transistor.start_link(token)
    assert Process.alive?(transistor)

    {:ok, cache} = Funnel.Caches.add token
    Funnel.Transistor.Cache.push(cache, 1, "plop")
    Funnel.Transistor.Cache.push(cache, 2, "plop")
    Funnel.Transistor.add(conn, token, 1)
  end

  test "send message from the cache" do
    token = "secrettoken"

    {:ok, cache} = Funnel.Caches.add token
    Funnel.Transistor.Cache.push(cache, 1, "plop")
    Funnel.Transistor.Cache.push(cache, 2, "plop")

    {:ok, _transistor} = Funnel.Transistor.start_link(token)
    Funnel.Transistor.add(self, token, 1)

    refute_receive({:chunk, %{:id => 1, :item => "plop"}})
    assert_receive({:chunk, %{:id => 2, :item => "plop"}})
  end

  test "send new message" do
    token = "secrettoken"

    {:ok, _transistor} = Funnel.Transistor.start_link(token)
    Funnel.Transistor.add(self, token, 1)
    Funnel.Transistor.notify(token, 2, "plup")

    assert_receive({:chunk, %{:id => 2, :item => "plup"}})
  end
end
