defmodule OhaiRouter do
  use Dynamo.Router
  filter JsonHeader

  post "/" do
    {:ok, body} = JSEX.encode([token: Funnel.Uuid.generate()])
    conn.resp 201, body
  end
end
