defmodule FilterRouter do
  use Dynamo.Router

  prepare do
    conn.fetch([:body])
  end

  post "/" do
    percolation =  Funnel.Es.put("/_percolator/funnel/message", conn.req_body)
    {:ok, body} = JSEX.decode(percolation.body)
    {:ok, response} = JSEX.encode([query_id: Funnel.Uuid.generate, body: body])
    conn = conn.put_resp_header "Content-Type", "application/json"
    conn.resp percolation.status_code, response
  end
end
