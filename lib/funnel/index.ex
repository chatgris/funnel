defmodule Funnel.Index do
  import Funnel.Responder

  @doc """
  Create an empty index.
  """
  def create do
    Funnel.Es.create |> respond
  end

  @doc """
  Create an index with mappings and settings.

  * `settings`     - Mappings and settings in json
  """
  def create(settings) do
    Funnel.Es.create(settings) |> respond
  end

  @doc """
  Delete an index.

  * `index_id` - Index's id
  """
  def destroy(index_id) do
    Funnel.Es.destroy(index_id) |> respond
  end
end
