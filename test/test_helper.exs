Dynamo.under_test(Funnel.Dynamo)
Dynamo.Loader.enable
ExUnit.start

defmodule Funnel.TestCase do
  use ExUnit.CaseTemplate

  # Enable code reloading on test cases
  setup do
    Dynamo.Loader.enable
    :ok
  end
end
