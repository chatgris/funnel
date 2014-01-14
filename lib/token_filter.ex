defmodule TokenFilter do
  @moduledoc """
  This module checks the presence of the `:token` params. Otherwise, it will
  halt the connection with `400` http return code
  """
  use Dynamo.Router

  def prepare(conn) do
    conn = conn.fetch([:params, :body])
    unless conn.params[:token] do
      halt! conn.status(400)
    end
    conn
  end
end
