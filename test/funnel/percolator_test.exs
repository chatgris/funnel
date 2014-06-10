defmodule Funnel.PercolatorTest do
  use Funnel.TestCase
  import Funnel.Es.Asserts, only: [assert_query_creation: 3]

  test "percolator is alive" do
    {:ok, percolator} = Funnel.Percolator.start_link(nil)
    assert Process.alive?(percolator)
  end

  test "notify transistor" do
    token = Funnel.register(self)
    index_id = "percolator_index"
    uuid = assert_query_creation('{"query" : {"match" : {"message" : "elasticsearch"}}}', index_id, token)["query_id"]
    message = "{\"doc\":{\"message\":\"this new elasticsearch percolator feature is nice, borat style\"}}"

    Funnel.Es.refresh
    Funnel.percolate(index_id, message)
    assert_receive({:chunk, %{id: _, item: item}})
    {:ok, item} = JSEX.decode item
    assert item["query_ids"] == [uuid]

    Funnel.Es.unregister(index_id, token, uuid)
  end

  test "notify transistor with a message matching several queries" do
    token = Funnel.register(self)
    index_id = "percolator_index_multiple"
    uuid1 = assert_query_creation('{"query" : {"match" : {"message" : "thanks"}}}', index_id, token)["query_id"]
    uuid2 = assert_query_creation('{"query" : {"match" : {"message" : "fish"}}}', index_id, token)["query_id"]
    message = "{\"doc\" : {\"message\":\"So long, and thanks for all the fish\"}}"

    Funnel.percolate(index_id, message)

    assert_receive({:chunk, %{id: _, item: item}})
    refute_receive({:chunk, %{id: _, item: _}})

    {:ok, item} = JSEX.decode item
    assert item["query_ids"] |> Enum.sort == [uuid1, uuid2] |> Enum.sort

    Funnel.Es.unregister(index_id, token, uuid1)
    Funnel.Es.unregister(index_id, token, uuid2)
    Funnel.Es.unregister(index_id)
  end

  test "notify transistor with a message matching several queries, bulk edition" do
    token = Funnel.register(self)
    index_id = "percolator_index_multiple"
    uuid1 = assert_query_creation('{"query" : {"match" : {"message" : "thanks"}}}', index_id, token)["query_id"]
    uuid2 = assert_query_creation('{"query" : {"match" : {"message" : "fish"}}}', index_id, token)["query_id"]
    message = "{\"doc\" : {\"message\":\"So long, and thanks for all the fish\"}}"
    message2 = "{\"doc\":{\"message\":\"Say thanks to the fish\"}}"

    Funnel.percolate(index_id, [message, message2])

    assert_receive({:chunk, %{id: _, item: item}})
    assert_receive({:chunk, %{id: _, item: item}})
    refute_receive({:chunk, %{id: _, item: _}})

    {:ok, item} = JSEX.decode item
    assert item["query_ids"] |> Enum.sort == [uuid1, uuid2] |> Enum.sort

    Funnel.Es.unregister(index_id, token, uuid1)
    Funnel.Es.unregister(index_id, token, uuid2)
    Funnel.Es.unregister(index_id)
  end
end
