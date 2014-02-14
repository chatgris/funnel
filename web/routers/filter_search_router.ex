defmodule FilterSearchRouter do
  use Dynamo.Router
  filter JsonHeader
  filter TokenFilter

  get "/" do
    {status_code, response} = Funnel.Es.find conn.params[:token], conn.params
    conn.resp status_code, response
  end
end
