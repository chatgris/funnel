ExUnit.start

defmodule Funnel.TestCase do
  use ExUnit.CaseTemplate

  # Enable code reloading on test cases
  setup do
    :ok
  end
end

defimpl Funnel.Transport, for: PID do
  def write(receiver, message) do
    send(receiver, {:chunk, message})
    {:ok, receiver}
  end
end

defmodule Funnel.Es.Asserts do
  import ExUnit.Assertions, only: [assert: 1, assert: 2]

  def create_index(index_id \\ "funnel") do
    Funnel.Es.destroy(index_id)
    settings = '{"settings" : {"number_of_shards" : 1},"mappings" : {"type1" : {"_source" : { "enabled" : false },"properties" : {"message" : { "type" : "string", "index" : "not_analyzed" },"field1" : { "type" : "string", "index" : "not_analyzed" }}}}}' |> IO.iodata_to_binary
    Funnel.Index.create(settings, index_id)
  end

  def assert_query_creation(query, index_id \\ "funnel", token \\ "token") do
    Funnel.Es.refresh
    {status, response} = Funnel.Es.register(index_id, token, query)
    {:ok, body} = Poison.decode response
    assert status == 201
    body
  end

  def assert_query_update(query, uuid, index \\ "funnel") do
    {status, response} = Funnel.Es.register(index, "token", uuid, query)
    {:ok, body} = Poison.decode response
    assert status == 200
    body
  end

  def assert_query_find(index, token \\ "token") do
    search_query = %{index_id: index}
    {status, response} = Funnel.Es.find(token, search_query)
    {:ok, response} = Poison.decode response
    assert status == 200
    response
  end

  def assert_percolate(%{"_id" =>id, "_index" => _}, uuid) do
    assert id == "token-#{uuid}"
  end
end
