defmodule JsonHeader do
  def prepare(conn) do
    conn.put_resp_header "Content-Type", "application/json"
  end
end
