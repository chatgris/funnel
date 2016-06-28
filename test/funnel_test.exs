defmodule FunnelTest do
  use Funnel.TestCase, async: true
  import Funnel.Es.Asserts

  def query do
    '{"query" : {"match" : {"message" : "elasticsearch"}}}'
      |> IO.iodata_to_binary
  end

  test "register a transport" do
    {:ok, token} = Funnel.register(self)
    assert token != nil
  end

  test "create an empty index" do
    {:ok, _status_code, body} = Funnel.Index.create
    index_id = body["index_id"]
    assert byte_size(index_id) == 32
    Funnel.Es.destroy(index_id)
  end

  test "create an index with settings" do
    within_index do
      assert byte_size(index_id) == 32
    end
  end

  test "delete an index" do
    within_index do
      Funnel.Es.refresh
      {:ok, status_code, body} = Funnel.Index.destroy(index_id)
      assert status_code == 200
      assert body != nil
    end
  end

  test "create a query" do
    within_index do
      assert_query_creation(query, index_id)
    end
  end

  test "update a query" do
    within_index do
      query_id = assert_query_creation(query, index_id)["query_id"]

      {:ok, status_code, body} = Funnel.Query.update(index_id, "token", query_id, query)
      assert status_code == 200
      assert body != nil
      Funnel.Query.destroy(index_id, "token", query_id)
    end
  end

  test "destroy a query" do
    within_index do
      query_id = assert_query_creation(query, index_id)["query_id"]
      {:ok, status_code, body} = Funnel.Query.destroy(index_id, "token", query_id)
      assert status_code == 200
      assert body != nil
    end
  end

  test "find a query from a token" do
    token = "super_mega_tok"

    within_index do
      query_id = assert_query_creation(query, index_id, token)["query_id"]
      Funnel.Es.refresh
      {:ok, status_code, body} = Funnel.Query.find(token)
      assert status_code == 200
      assert Enum.count(body) == 1
      Funnel.Query.destroy(index_id, token, query_id)
    end
  end

  test "find a query from a token and index_id" do
    token = "super_mega_token"

    within_index do
      query_id = assert_query_creation(query, index_id, token)["query_id"]
      Funnel.Es.refresh
      {:ok, _status_code, _body} = Funnel.Query.find(token)
      {:ok, status_code, body} = Funnel.Query.find(token, %{index_id: index_id})
      assert status_code == 200
      assert Enum.count(body) == 1
      Funnel.Query.destroy(index_id, token, query_id)
    end
  end

  test "submit a message to the percolator" do
    message = '{"message" : "this new elasticsearch percolator feature is nice, borat style"}' |> IO.iodata_to_binary
    assert Funnel.percolate("funnel", message) == {:ok}
  end
end
