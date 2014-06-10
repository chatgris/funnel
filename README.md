# Funnel

[![Build Status](https://travis-ci.org/AF83/funnel.png?branch=master)](https://travis-ci.org/AF83/funnel)

Funnel is for building Streaming APIs build upon ElasticSearch’s
[percolation](http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-percolate.html).

Funnel supports ElasticSearch >= 1.1.

Funnel allow to register users / devices, associates some queries to user.

The common usecase is to store a query from a user and notify this user when a
new document matching this query is available.

## Installing things

You can use Funnel in your projects with the following steps:

1. Adding Funnel to your `mix.exs` dependencies:

```elixir
def deps do
  [
    {:funnel, github: "AF83/funnel"},
  ]
end
```

2. List the `:funnel` as your application dependencies:

```elixir
def application do
  [applications: [:funnel]]
end
```

## Testing things

``` shell
mix test
```

## Doing things

### Transport

Funnel has this notion of `Funnel.Transport`. Anything implementing the
`Funnel.Transport` protocol can be a transport.

For the dynamo framework, the protocol would looks like:

```elixir
defmodule EventStreamMessage do
  @moduledoc ""
  This module serialize a given id and body to a ServerSent Events message.
  ""
  def to_message(id, data) do
    "id:#{id}\ndata: #{data}\n\n"
  end
end

defimpl Funnel.Transport, for: Elixir.Dynamo.Cowboy.Connection do
  def write(conn, %{:id => id, :item => item}) do
    conn.chunk EventStreamMessage.to_message(id, item)
  end
end
```

### Register

A user, or a device, can register to funnel by using the `/register` endpoint.
This will return a token. This token must be used in all communications with the
funnel's API.

```elixir
Funnel.register(transport)
Funnel.register(transport, last_id)
```
