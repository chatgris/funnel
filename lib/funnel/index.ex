defmodule Funnel.Index do
  import Funnel.Responder
  require Logger

  @doc """
  Create an empty index.
  """
  def create do
    Logger.debug "[Index] : Create new index"
    Funnel.Es.create |> respond
  end

  @doc """
  Create an index with mappings and settings.

  * `settings`     - Mappings and settings in json
  """
  def create(settings) do
    Logger.debug "[Index] Create index with settings: #{settings}"
    Funnel.Es.create(settings) |> respond
  end

  @doc """
  Create an index with mappings and settings.

  * `settings`     - Mappings and settings in json
  """
  def create(settings, index_id) do
    Logger.debug "[Index] Create index with settings: #{settings}"
    Funnel.Es.create(settings, index_id) |> respond
  end

  @doc """
  Delete an index.

  * `index_id` - Index's id
  """
  def destroy(index_id) do
    Logger.debug "[Index] : Destroy #{index_id}"
    Funnel.Es.destroy(index_id) |> respond
  end
end
