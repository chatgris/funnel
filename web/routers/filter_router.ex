defmodule FilterRouter do
  use Dynamo.Router
  filter JsonHeader
  filter TokenFilter

  post "/" do
    {status_code, response} = Funnel.Es.register conn.params[:token], conn.req_body
    conn.resp status_code, response
  end

  put ":uuid" do
    {status_code, response} = Funnel.Es.register conn.params[:token], conn.params[:uuid], conn.req_body
    conn.resp status_code, response
  end

  delete ":uuid" do
    {status_code, response} = Funnel.Es.unregister conn.params[:token], conn.params[:uuid]
    conn.resp status_code, response
  end
end
