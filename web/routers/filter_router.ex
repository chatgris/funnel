defmodule FilterRouter do
  use Dynamo.Router

  prepare do: conn.fetch([:body])

  post "/" do
    {status_code, response} = Funnel.Es.percolate conn.req_body
    conn = conn.put_resp_header "Content-Type", "application/json"
    conn.resp status_code, response
  end
end
