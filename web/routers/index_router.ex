defmodule IndexRouter do
  use Dynamo.Router
  filter JsonHeader
  filter TokenFilter

  post "/" do
    {status_code, response} = Funnel.Es.create conn.req_body
    conn.resp status_code, response
  end
end
