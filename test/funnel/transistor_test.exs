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

  test "send message from the cache" do
    token = "secrettoken"

    {:ok, cache} = Funnel.Caches.add token
    Funnel.Transistor.Cache.push(cache, 1, "plop")
    Funnel.Transistor.Cache.push(cache, 2, "plop")

    {:ok, _transistor} = Funnel.Transistor.start_link(token)
    Funnel.Transistor.add(self, token, 1)

    refute_receive({:chunk, [id: 1, body: "plop"]})
    assert_receive({:chunk, [id: 2, body: "plop"]})
  end

  test "send new message" do
    token = "secrettoken"

    {:ok, _transistor} = Funnel.Transistor.start_link(token)
    Funnel.Transistor.add(self, token, 1)
    Funnel.Transistor.notify(token, 2, "plup")

    assert_receive({:chunk, [id: 2, body: "plup"]})
  end
end
