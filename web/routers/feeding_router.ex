defmodule FeedingRouter do
  use Dynamo.Router
  filter JsonHeader

  prepare do: conn.fetch([:params, :body])

  post "/" do
    Funnel.percolate conn.params[:index_id], conn.req_body
    conn.resp 202, ""
  end
end
