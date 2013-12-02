defmodule OhaiRouter do
  use Dynamo.Router

  post "/" do
    conn = conn.put_resp_header "Content-Type", "application/json"
    {:ok, body} = JSEX.encode([token: Funnel.Uuid.generate()])
    conn.resp 201, body
  end
end
