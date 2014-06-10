defprotocol Funnel.Transport do
  @doc "Write a message"
  def write(receiver, message)
end
