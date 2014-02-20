defmodule RiverRouter do
  use Dynamo.Router
  filter EventStreamHeader
  filter TokenFilter

  get "/" do
    {:ok, _transistor} = Funnel.Transistors.add conn.params[:token]
    conn = Funnel.Transistor.add(conn, conn.params[:token])
    await(conn, &on_wake_up(&1, &2))
  end

  defp on_wake_up(_message, conn) do
    await(conn, &on_wake_up(&1, &2))
  end
end
