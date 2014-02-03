defmodule FeedingRouter do
  use Dynamo.Router
  filter JsonHeader

  prepare do: conn.fetch([:params, :body])

  post "/" do
    Funnel.Percolator.percolate conn.params[:index_id], conn.req_body
    conn.resp 204, ""
  end
end
