defmodule QuerySearchRouter do
  use Dynamo.Router
  filter JsonHeader
  filter TokenFilter

  get "/" do
    {status_code, response} = Funnel.Es.find conn.assigns[:token], conn.params
    conn.resp status_code, response
  end
end
