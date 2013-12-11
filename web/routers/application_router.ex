defmodule ApplicationRouter do
  use Dynamo.Router
  filter JsonHeader

  forward "/status", to: StatusRouter
  forward "/ohai",   to: OhaiRouter
  forward "/filter", to: FilterRouter

  get "/" do
    {:ok, payload} = JSEX.encode [message: "Welcome to funnel"]
    conn.resp 200, payload
  end
end
