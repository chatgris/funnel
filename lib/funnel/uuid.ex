defmodule Funnel.Uuid do
  @module_doc """

  Wrapper around uuid library.
  """

  @doc """

  Returns a sweet uuid.
  """
  def generate do
    UUID.uuid4(:hex)
  end
end
