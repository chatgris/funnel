defmodule Funnel.EsTest do
  use Funnel.TestCase, async: true

  test "Returns Elasticsearc status" do
    response = Funnel.Es.get("/")
    {:ok, payload} = JSEX.decode(response.body)
    assert payload["status"] == 200
  end
end
