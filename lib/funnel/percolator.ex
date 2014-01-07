defmodule Funnel.Percolator do
  use ExActor, export: :percolator

  def percolate(body) do
    Funnel.Es.percolate(body)
      |> Enum.each(fn(match)-> notify(match, body) end)
    { :noreply, nil}
  end

  defp notify(match, body) do
    [token, uuid] = String.split(match, "-")
    Funnel.Transistor.notify(token, uuid, body)
  end
end
