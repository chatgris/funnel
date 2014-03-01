defmodule ApplicationRouter do
  use Dynamo.Router
  filter JsonHeader

  forward "/status",   to: StatusRouter
  forward "/register", to: RegisterRouter
  forward "/feeding",  to: FeedingRouter
  forward "/river",    to: RiverRouter
  forward "/index",    to: IndexRouter
  forward "/queries",  to: QuerySearchRouter

  get "/" do
    {:ok, payload} = JSEX.encode [message: "Welcome to funnel"]
    conn.resp 200, payload
  end
end
