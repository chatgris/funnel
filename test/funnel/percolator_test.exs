defmodule Funnel.PercolatorTest do
  use Funnel.TestCase, async: true

  test "percolator is alive" do
    { :error, {:already_started, percolator}} = Funnel.Percolator.start_link
    assert Process.alive?(percolator)
  end
end
