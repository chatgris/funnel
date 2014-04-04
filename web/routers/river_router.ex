defmodule RiverRouter do
  use Dynamo.Router
  filter EventStreamHeader
  filter TokenFilter

  get "/" do
    {:ok, _transistor} = Funnel.Transistors.add conn.params[:token]
    conn = conn.send_chunked(200)
    conn = Funnel.Transistor.add(conn, conn.params[:token], conn.params[:last_id])
    await(conn, &on_wake_up(&1, &2))
  end

  defp on_wake_up(_message, conn) do
    await(conn, &on_wake_up(&1, &2))
  end
end
