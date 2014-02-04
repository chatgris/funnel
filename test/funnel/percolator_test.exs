defmodule Funnel.PercolatorTest do
  use Funnel.TestCase, async: true

  test "percolator is alive" do
    percolator = case Funnel.Percolator.start_link(nil) do
      {:error, {:already_started, percolator}} ->
        percolator
      {:ok, percolator} ->
        percolator
    end
    assert Process.alive?(percolator)
  end
end
