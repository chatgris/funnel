Dynamo.under_test(Funnel.Dynamo)
Dynamo.Loader.enable
ExUnit.start

defmodule Funnel.TestCase do
  use ExUnit.CaseTemplate

  # Enable code reloading on test cases
  setup do
    Dynamo.Loader.enable
    :ok
  end
end

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

defmodule Funnel.Es.Asserts do
  import ExUnit.Assertions, only: [assert: 1, assert: 2]

  def assert_query_creation(query, index \\ "funnel") do
    {status, response} = Funnel.Es.register(index, "token", query)
    {:ok, body} = JSEX.decode response
    assert status == 201
    body
  end

  def assert_query_update(query, uuid, index \\ "funnel") do
    {status, response} = Funnel.Es.register(index, "token", uuid, query)
    {:ok, body} = JSEX.decode response
    assert status == 200
    body
  end

  def assert_query_find(index) do
    hash = HashDict.new
    hash = Dict.put(hash, :index_id, index)
    {status, response} = Funnel.Es.find("token", hash)
    {:ok, response} = JSEX.decode response
    assert status == 200
    response
  end

  def assert_query_find do
    {status, response} = Funnel.Es.find("token")
    {:ok, response} = JSEX.decode response
    assert status == 200
    response
  end

  def assert_percolate(%{"_id" =>id, "_index" => _}, uuid) do
    assert id == "token-#{uuid}"
  end
end
