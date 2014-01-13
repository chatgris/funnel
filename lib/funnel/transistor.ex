defmodule Funnel.Transistor do
  use GenServer.Behaviour

  def start_link(conn) do
    :gen_server.start_link({:local, name(conn)}, __MODULE__, [], [])
  end

  def notify(token, match, body) do
    :gen_server.cast binary_to_atom(token), {:notify, match, body}
  end

  def add(conn) do
    conn = conn.send_chunked(200)
    :gen_server.call name(conn), {:add, conn}
  end

  def init([]) do
    {:ok, []}
  end

  def handle_cast({:notify, match, body}, connexions) do
    {:ok, response} = JSEX.encode([filter_id: match, body: body])
    {:noreply, Enum.reduce(connexions, [], fn(conn, acc) -> write(acc, conn, response) end) }
  end

  def handle_call({:add, conn}, _from, connexions) do
    {:reply, conn, [conn | connexions]}
  end

  defp name(conn) do
    binary_to_atom(conn.params[:token])
  end

  defp write(acc, conn, body) do
    filter acc, conn.chunk "data: #{body}\n\n"
  end

  defp filter(acc, {:ok, conn}) do
    [conn | acc]
  end

  defp filter(acc, {:error, _}) do
    acc
  end
end
