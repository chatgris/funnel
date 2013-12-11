defmodule StatusRouter do
  use Dynamo.Router
  filter JsonHeader

  get "/" do
    conn.resp 200, Funnel.Es.get("/").body
  end
end
