defmodule Funnel.Transistor do
  use GenServer.Behaviour

  def start_link(conn) do
    conn = conn.send_chunked(200)
    {conn, :gen_server.start_link({:local, name(conn)}, __MODULE__, conn, []) }
  end

  def notify(token, match, body) do
    :gen_server.cast binary_to_atom(token), {:notify, match, body}
  end

  def init(conn) do
    {:ok, conn}
  end

  def handle_cast({:notify, match, body}, conn) do
    {:ok, response} = JSEX.encode([filter_id: match, body: body])
    {:ok, conn} = conn.chunk "data: #{response}\n\n"
    {:noreply, conn}
  end

  defp name(conn) do
    binary_to_atom(conn.params[:token])
  end
end
