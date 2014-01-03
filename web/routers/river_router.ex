defmodule RiverRouter do
  use Dynamo.Router
  filter EventStreamHeader
  filter TokenFilter

  get "/" do
    {conn, _} = Funnel.Transistor.start_link conn
    await(conn, &on_wake_up(&1, &2))
  end

  defp on_wake_up(_message, conn) do
    await(conn, &on_wake_up(&1, &2))
  end
end
