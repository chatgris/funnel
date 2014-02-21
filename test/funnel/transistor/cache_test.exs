defmodule Funnel.Transistor.CacheTest do
  use Funnel.TestCase, async: true
  alias Funnel.Transistor.Cache

  test "process is alive" do
    {:ok, pid} = Cache.start_link("alive")
    assert Process.alive?(pid)
  end

  test "adds item to the cache" do
    {:ok, pid} = Cache.start_link("items")
    Cache.push(pid, "Hello")
    item = List.first(Cache.list(pid))
    assert item == [id: 0, item: "Hello"]
    Cache.push(pid, "Bye")
    item = List.last(Cache.list(pid))
    assert item == [id: 1, item: "Bye"]
  end

  test "limits size of the cache" do
    {:ok, pid} = Cache.start_link("limits")
    range = Range.new(0, 100)
    Enum.each range, fn(_) -> Cache.push(pid, "Hello") end
    assert Enum.count(Cache.list(pid)) == 10
    Cache.push(pid, "Bye")
    item = List.last(Cache.list(pid))
    assert item == [id: 101, item: "Bye"]
  end

  test "return a list from a given id" do
    {:ok, pid} = Cache.start_link("from_id")
    range = Range.new(0, 100)
    Enum.each range, fn(_) -> Cache.push(pid, "Hello") end
    assert Enum.count(Cache.list(pid, 97)) == 3
  end

  test "return a list from an id older than cache" do
    {:ok, pid} = Cache.start_link("older")
    range = Range.new(0, 100)
    Enum.each range, fn(_) -> Cache.push(pid, "Hello") end
    assert Enum.count(Cache.list(pid, 0)) == 10
  end

  test "return a list with an unknow id" do
    {:ok, pid} = Cache.start_link("unknow")
    range = Range.new(0, 100)
    Enum.each range, fn(_) -> Cache.push(pid, "Hello") end
    assert Enum.count(Cache.list(pid, 443662456)) == 0
  end
end
