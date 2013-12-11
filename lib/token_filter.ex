defmodule TokenFilter do
  use Dynamo.Router

  def prepare(conn) do
    conn = conn.fetch([:params, :body])
    unless conn.params[:token] do
      halt! conn.status(400)
    end
    conn
  end
end
