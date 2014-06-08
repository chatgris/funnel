defmodule Funnel.Index do
  @doc """
  Create an empty index.
  """
  def create do
    Funnel.Es.create
  end

  @doc """
  Create an index with mappings and settings.

  * `body`     - Mappings and settings in json
  """
  def create(options) do
    Funnel.Es.create(options)
  end

  @doc """
  Delete an index.

  * `index_id` - Index's id
  """
  def destroy(index_id) do
    Funnel.Es.destroy(index_id)
  end
end
