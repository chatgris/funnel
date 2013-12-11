defmodule FeedingRouter do
  use Dynamo.Router
  filter JsonHeader

  prepare do: conn.fetch(:body)

  post "/" do
    Funnel.Es.percolate conn.req_body
    conn.resp 204, nil
  end
end
