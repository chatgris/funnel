defmodule Funnel.Transistor.CacheTest do
  use Funnel.TestCase
  alias Funnel.Transistor.Cache

  defp fill_cache(pid) do
    range = Range.new(0, 100)
    Enum.each range, fn(id) -> Cache.push(pid, id, "Hello") end
  end

  test "process is alive" do
    {:ok, pid} = Cache.start_link("alive")
    assert Process.alive?(pid)
  end

  test "adds item to the cache" do
    {:ok, pid} = Cache.start_link("items")
    Cache.push(pid, "toto", "Hello")
    item = List.first(Cache.list(pid))
    assert item == [id: "toto", item: "Hello"]
    Cache.push(pid, "roger", "Bye")
    item = List.last(Cache.list(pid))
    assert item == [id: "roger", item: "Bye"]
  end

  test "limits size of the cache" do
    {:ok, pid} = Cache.start_link("limits")
    fill_cache(pid)
    assert Enum.count(Cache.list(pid)) == 10
    Cache.push(pid, 102, "Bye")
    item = List.last(Cache.list(pid))
    assert item == [id: 102, item: "Bye"]
  end

  test "return a list from a given id" do
    {:ok, pid} = Cache.start_link("from_id")
    fill_cache(pid)
    assert Enum.count(Cache.list(pid, 97)) == 3
  end

  test "return a list from an id which is not an integer" do
    {:ok, pid} = Cache.start_link("items2")
    Cache.push(pid, "toto", "Hello")
    Cache.push(pid, "roger", "Bye")
    items = Cache.list(pid, "toto")
    assert items == [[id: "roger", item: "Bye"]]
  end

  test "return a list with an unknow id" do
    {:ok, pid} = Cache.start_link("unknow")
    fill_cache(pid)
    assert Enum.count(Cache.list(pid, 443662456)) == 0
  end
end
