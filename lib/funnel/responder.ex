defmodule Funnel.Responder do
  def respond({status_code, body}) do
    {:ok, response}     = Poison.decode(body)
    {:ok, status_code, response}
  end
end
