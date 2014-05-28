defmodule Funnel.PercolatorTest do
  use Funnel.TestCase
  import Funnel.Es.Asserts, only: [assert_query_creation: 2]

  test "percolator is alive" do
    {:ok, percolator} = Funnel.Percolator.start_link(nil)
    assert Process.alive?(percolator)
  end

  test "notify transistor" do
    token = "token"
    index_id = "percolator_index"
    uuid = assert_query_creation('{"query" : {"match" : {"message" : "elasticsearch"}}}', index_id)["query_id"]
    message = '{"doc" : {"message" : "this new elasticsearch percolator feature is nice, borat style"}}'

    Funnel.register(self, token)
    Funnel.Es.refresh
    Funnel.percolate(index_id, message)
    assert_receive({:chunk, %{id: _, item: item}})
    {:ok, item} = JSEX.decode item
    assert item["query_id"] == uuid

    Funnel.Es.unregister(index_id, token, uuid)
  end
end
