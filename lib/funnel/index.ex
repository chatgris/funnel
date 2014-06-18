defmodule Funnel.Index do
  @doc """
  Create an empty index.
  """
  def create do
    {status_code, body} = Funnel.Es.create
    {:ok, status_code, body}
  end

  @doc """
  Create an index with mappings and settings.

  * `settings`     - Mappings and settings in json
  """
  def create(settings) do
    {status_code, body} = Funnel.Es.create(settings)
    {:ok, status_code, body}
  end

  @doc """
  Delete an index.

  * `index_id` - Index's id
  """
  def destroy(index_id) do
    {status_code, body} = Funnel.Es.destroy(index_id)
    {:ok, status_code, body}
  end
end
