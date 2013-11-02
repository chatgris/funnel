defmodule ApplicationRouter do
  use Dynamo.Router

  prepare do
    # Pick which parts of the request you want to fetch
    # You can comment the line below if you don't need
    # any of them or move them to a forwarded router
    conn.fetch([:cookies, :params])
  end

  # It is common to break your Dynamo into many
  # routers, forwarding the requests between them:
  # forward "/posts", to: PostsRouter

  get "/" do
    conn = conn.put_resp_header "Content-Type", "application/json"
    {:ok, payload} = JSEX.encode [message: "Welcome to funnel"]
    conn.resp 200, payload
  end
end
