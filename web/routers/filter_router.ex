defmodule FilterRouter do
  use Dynamo.Router

  filter JsonHeader

  prepare do: conn.fetch([:body, :params])

  post "/" do
    {status_code, response} = Funnel.Es.percolate conn.req_body
    conn.resp status_code, response
  end

  put ":uuid" do
    {status_code, response} = Funnel.Es.percolate conn.params[:uuid], conn.req_body
    conn.resp status_code, response
  end

  delete ":uuid" do
    {status_code, response} = Funnel.Es.unpercolate conn.params[:uuid]
    conn.resp status_code, response
  end
end
