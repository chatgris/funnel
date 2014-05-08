defmodule EventStreamMessageTest do
  use Funnel.TestCase

  test "serialize a given id and data to a SSE message" do
    assert EventStreamMessage.to_message(1, "SSE") == "id:1\ndata: SSE\n\n"
  end
end
