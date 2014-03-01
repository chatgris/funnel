defmodule IndexRouter do
  use Dynamo.Router
  filter JsonHeader
  filter TokenFilter

  forward "/:index_id", to: QueryRouter
  prepare do: conn.fetch(:body)

  post "/" do
    {status_code, response} = Funnel.Es.create conn.req_body
    conn.resp status_code, response
  end
end
