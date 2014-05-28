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
    Funnel.percolate(index_id, message)
    Funnel.Es.unregister(index_id)
    Funnel.Es.unregister(index_id, "token", uuid)

    assert_receive({:chunk, %{id: uuid, item: _}}, 500)
  end
end
