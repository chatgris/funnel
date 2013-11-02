defmodule StatusRouter do
  use Dynamo.Router

  get "/" do
    conn = conn.put_resp_header "Content-Type", "application/json"
    conn.resp 200, Funnel.Es.get("/").body
  end
end
