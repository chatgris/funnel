defmodule EventStreamHeader do
  @moduledoc """
  This module set the response `Content-Type` Header to `text/events-stream`,
  the ServerSent Events header.
  """
  def prepare(conn) do
    conn.put_resp_header "Content-Type", "text/events-stream"
  end
end
