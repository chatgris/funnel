defprotocol Funnel.Transport do
  @doc "Write a message"
  def write(receiver, message)
end

defimpl Funnel.Transport, for: Elixir.Dynamo.Cowboy.Connection do
  def write(conn, %{:id => id, :item => item}) do
    conn.chunk EventStreamMessage.to_message(id, item)
  end
end
