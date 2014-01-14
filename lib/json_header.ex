defmodule JsonHeader do
  @moduledoc """
  This module set the response `Content-Type` Header to `application/json`,
  """
  def prepare(conn) do
    conn.put_resp_header "Content-Type", "application/json"
  end
end
