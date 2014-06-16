defmodule Funnel.Percolator do
  @moduledoc """
  `Funnel.Percolator` is meant to receive documents, send those documents to the
  Elasticsearch percolation, then, notify each User.
  """

  use GenServer

  @doc """

  Start a new `Funnel.Percolator` actor.
  """
  def start_link(_state) do
    GenServer.start_link(__MODULE__, nil, [])
  end

  @doc """
  Wrapper around `GenServer`. Send a document to Elasticsearch's percolator.

  * `percolator`   - PID of the current percolator
  * `index_id`     - Index's id
  * `document`     - Document in json, can be a document, or a list of documents
  """
  def percolate(percolator, index_id, document) do
    {:ok, document} = JSEX.decode(document)
    do_percolate(percolator, index_id, document)
  end

  @doc """
  Default values of `Funnel.Percolator`.
  """
  def init do
    {:ok, nil}
  end

  @doc """

  Percolates and notify on each match.
  """
  def handle_cast({:percolate, index_id, body}, nil) do
    Funnel.Es.percolate(index_id, body)
      |> group_by_token
      |> Enum.each(fn(match)-> notify(match, body) end)
    { :noreply, nil}
  end

  def do_percolate(percolator, index_id, documents) when is_list(documents) do
    Enum.each(documents, fn(document)-> do_percolate(percolator, index_id, document) end)
  end

  def do_percolate(percolator, index_id, document) when is_map(document) do
    {:ok, document} = JSEX.encode(document)
    GenServer.cast(percolator, {:percolate, index_id, document})
  end

  defp group_by_token(collection) do
    Enum.reduce(collection, [], fn(match, matches) ->
      [token, uuid] = decode_match(match)
      match = extract_match(matches, token)
      ids = [uuid | match[:query_ids]]
      matches = List.delete(matches, match)

      [%{match | :query_ids => ids} | matches]
    end)
  end

  defp extract_match(matches, token) do
    case Enum.find(matches, fn(x) -> x[:token] == token end) do
      nil  -> %{:token => token, :query_ids => []}
      elem -> elem
    end
  end

  defp notify(match, body) do
    token = match[:token]
    id = Funnel.Uuid.generate

    {:ok, cache} = Funnel.Caches.add token
    {:ok, response} = JSEX.encode([query_ids: match[:query_ids], body: body])

    Funnel.Transistor.Cache.push(cache, id, response)
    Funnel.Transistor.notify(token, id, response)
  end

  defp decode_match(match) do
    String.split(match["_id"], "-")
  end
end
