# Funnel

[![Build Status](https://travis-ci.org/AF83/funnel.png?branch=master)](https://travis-ci.org/AF83/funnel)

Funnel is for building Streaming APIs build upon ElasticSearch’s
[percolation](http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-percolate.html).

Funnel supports ElasticSearch >= 1.1.

Funnel allow to register users / devices, associates some queries to user.

The common usecase is to store a query from a user and notify this user when a
new document matching this query is available.

Check out this [example](https://github.com/AF83/funnel_http) for a complete http api using Funnel dsl.

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

The first argument is a `transport`, the second one is optionnal, and allow to
write onto the transport the message seen since the `last_id given`.

```elixir
{:ok, token} = Funnel.register(transport)
{:ok, "1a9dc09879374878bd7aab27c7be6bc7"}

{:ok, token} = Funnel.register(self, "422f779c759244d4aad45ac94c83b7da")
{:ok, "80cfd5a3e1324db8b076defec2ddc1b2"}
```

### Creating Indexes

This example will create an empty index:

```elixir
{:ok, status_code, body} = Funnel.Index.create
{:ok, 200, "{\"index_id\":\"e431710007a640fa947eba4f40b00e0f\",\"body\":{\"acknowledged\":true}}"}
```

This example will create an index with specific settings:

``` elixir
settings = '{"settings" : {"number_of_shards" : 1},"mappings" : {"type1" :{"_source" : { "enabled" : false },"properties" : {"field1" : { "type" :"string", "index" : "not_analyzed" }}}}}' |> IO.iodata_to_binary
{:ok, _status_code, body} = Funnel.Index.create(settings)
{:ok, 200,"{\"index_id\":\"a87713a4a284414f970297b68a08fc9c\",\"body\":{\"acknowledged\":true}}"}
```


### Deleting an index

``` elixir
{:ok, status_code, body} = Funnel.Index.destroy("422c964929fd4f39a85eca541341984a")
{:ok, 200, "{\"acknowledged\":true}"}
```

### Creating a query

``` elixir
query = '{"query" : {"match" : {"message" : "funnel"}}}' |> IO.iodata_to_binary
{:ok, status_code, body} = Funnel.Query.create(index_id, token, query)
{:ok, 201, "{\"query_id\":\"2cf60bc676cb496996c16e405011750b\",\"index_id\":\"936f7a079cec4d59b23a08f4089af9b3\",\"body\":{\"_id\":\"token-2cf60bc676cb496996c16e405011750b\",\"_index\":\"936f7a079cec4d59b23a08f4089af9b3_test\",\"_type\":\".percolator\",\"_version\":1,\"created\":true}}"}
```

#### Updating a query

``` elixir
{:ok, status_code, body} = Funnel.Query.update(index_id, token, query_id, query)
{:ok, 200, "{\"query_id\":\"2cf60bc676cb496996c16e405011750b\",\"index_id\":\"936f7a079cec4d59b23a08f4089af9b3\",\"body\":{\"_id\":\"token-2cf60bc676cb496996c16e405011750b\",\"_index\":\"936f7a079cec4d59b23a08f4089af9b3_test\",\"_type\":\".percolator\",\"_version\":2,\"created\":false}}"}
```

#### Deleting a query

``` elixir
{:ok, status_code, body} = Funnel.Query.destroy(index_id, token, query_id)
:ok, 200, "{\"found\":true,\"_index\":\"529546cb498c4efea03457888e4a86ad_test\",\"_type\":\".percolator\",\"_id\":\"token-f32dd53f203f4cc1a7b9e082fa5fcfcf\",\"_version\":2}"}
```

#### Finding queries

``` elixir
{:ok, status_code, body} = Funnel.Query.find(token)
:ok, 200, "[{\"query_id\":\"4f122313862e494b8810f073c27cf43d\",\"index_id\":\"b79d2e9ff8c949e08ba98c4d8c216547\",\"score\":1.0}]"}

{:ok, status_code, body} = Funnel.Query.find(token, %{index_id: index_id})
:ok, 200, "[{\"query_id\":\"4f122313862e494b8810f073c27cf43d\",\"index_id\":\"b79d2e9ff8c949e08ba98c4d8c216547\",\"score\":1.0}]"}
```

#### Submitting a document to the percolator

``` elixir
message = '{"message" : "this new elasticsearch percolator feature is nice, borat style"}' |> IO.iodata_to_binary
Funnel.percolate(index_id, message)
{:ok}
```
