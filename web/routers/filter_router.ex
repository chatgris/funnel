defmodule FilterRouter do
  use Dynamo.Router
  filter JsonHeader
  filter TokenFilter
  forward "/feeding", to: FeedingRouter

  get "/" do
    {status_code, response} = Funnel.Es.find conn.params[:token], conn.params
    conn.resp status_code, response
  end

  post "/" do
    {status_code, response} = Funnel.Es.register conn.params[:index_id], conn.params[:token], conn.req_body
    conn.resp status_code, response
  end

  put ":uuid" do
    {status_code, response} = Funnel.Es.register conn.params[:index_id], conn.params[:token], conn.params[:uuid], conn.req_body
    conn.resp status_code, response
  end

  delete ":uuid" do
    {status_code, response} = Funnel.Es.unregister conn.params[:index_id], conn.params[:token], conn.params[:uuid]
    conn.resp status_code, response
  end
end
