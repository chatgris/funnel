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
