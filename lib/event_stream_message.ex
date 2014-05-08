defmodule EventStreamMessage do
  @moduledoc """
  This module serialize a given id and body to a ServerSent Events message.
  """
  def to_message(id, data) do
    "id:#{id}\ndata: #{data}\n\n"
  end
end
