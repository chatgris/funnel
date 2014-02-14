defmodule FilterRouter do
  use Dynamo.Router
  filter JsonHeader
  filter TokenFilter
  forward "/feeding", to: FeedingRouter
  forward "/filters", to: FilterSearchRouter

  post "/" do
    {status_code, response} = Funnel.Es.register conn.params[:index_id], conn.assigns[:token], conn.req_body
    conn.resp status_code, response
  end

  put ":uuid" do
    {status_code, response} = Funnel.Es.register conn.params[:index_id], conn.assigns[:token], conn.params[:uuid], conn.req_body
    conn.resp status_code, response
  end

  delete ":uuid" do
    {status_code, response} = Funnel.Es.unregister conn.params[:index_id], conn.assigns[:token], conn.params[:uuid]
    conn.resp status_code, response
  end
end
