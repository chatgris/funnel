defmodule IndexRouter do
  use Dynamo.Router
  filter JsonHeader
  filter TokenFilter

  forward "/:index_id", to: FilterRouter

  post "/" do
    {status_code, response} = Funnel.Es.create conn.req_body
    conn.resp status_code, response
  end
end
