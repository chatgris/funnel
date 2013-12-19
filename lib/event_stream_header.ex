defmodule EventStreamHeader do
  def prepare(conn) do
    conn.put_resp_header "Content-Type", "text/events-stream"
  end
end
