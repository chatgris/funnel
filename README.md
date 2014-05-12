# Funnel

[![Build Status](https://travis-ci.org/AF83/funnel.png?branch=master)](https://travis-ci.org/AF83/funnel)

Funnel is for building Streaming APIs build upon ElasticSearch’s
[percolation](http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-percolate.html).

Funnel supports ElasticSearch >= 1.1.

Funnel allow to register users / devices, associates some queries to user, and
exposes a streaming endpoint for each user.

The common usecase is to store a query from a user and notify this user when a
new document matching this query is available.

## Installing things

First, Elixir must be installed, check out the resources section.

Once Elixir is installed, dependencies must be installed with:

``` shell
mix do deps.get, deps.compile
```

## Running things

Starting the HTTP API:

``` shell
mix server
```

## Testing things

``` shell
mix test
```

## Doing things


### Register

A user, or a device, can register to funnel by using the `/register` endpoint.
This will return a token. This token must be used in all communications with the
funnel's API.

``` shell
curl -H "Content-Type: application/json" -H "Accept: application/json" -XPOST http://localhost:4000/register
```
``` json
{"token":"7d0ac81fbdd646dd9e883e3b007ce58d"}
```

The token can be passed as a parameter, or by using the Authorization header.

For the sake of readability, we assume those headers for all subsequent
examples:

``` shell
-H "Content-Type: application/json" -H "Accept: application/json" -H "Authorization: 7d0ac81fbdd646dd9e883e3b007ce58d"
```

### Index

Ok, now, let's create an index:

``` shell
curl -XPOST "http://localhost:4000/index" -d '{"settings" : {"number_of_shards" : 1},"mappings" : {"type1" : {"_source" : {"enabled" : false},"properties" :{"field1":{"type":"string","index":"not_analyzed"}}}}}'
```
``` json
{"index_id":"bfa3e5b02e554b458165815968ed490b","body":{"ok":true,"acknowledged":true}}
```

### Query

#### Adding query

Adding queries is done by using the `/query` endpoint. The payload must
comply with the funnel's query serialization. These entries can accept a single
query, or a list of queries.

A query is defined by a user's token, a name, and a json string representing the
elasticsearch query.

``` shell
curl -XPOST "http://localhost:4000/index/bfa3e5b02e554b458165815968ed490b" -d '{"query" : {"term" : {"field1" : "value1"}}}'
```
``` json
{"query_id":"dac278b8a6904b469d85df0773d16f5a","body":{"ok":true,"_index":"_percolator","_type":"bfa3e5b02e554b458165815968ed490b_dev","_id":"7d0ac81fbdd646dd9e883e3b007ce58d-dac278b8a6904b469d85df0773d16f5a","_version":1}}
```

#### Searching queries

Queries can be retrieved for a given `index_id` with the following:

``` shell
curl -XGET "http://localhost:4000/index/bfa3e5b02e554b458165815968ed490b/queries"
```
``` json
[{"query_id":"dac278b8a6904b469d85df0773d16f5a","index_id":"bfa3e5b02e554b458165815968ed490b","score":1.4142135}]
```

Queries can be retrieved for a given `token` with the following:

``` shell
curl -XGET "http://localhost:4000/queries"
```
``` json
[{"query_id":"dac278b8a6904b469d85df0773d16f5a","index_id":"bfa3e5b02e554b458165815968ed490b","score":1.4142135}]
```

### Submiting documents

Adding messages is done by using the `/feeding` endpoint. The payload must
comply with the funnel's message serialization.


``` shell
curl -XPOST "http://localhost:4000/index/bfa3e5b02e554b458165815968ed490b/feeding" -d '{"doc":{"field1" : "value1"}}'
```

### Streaming

Listening to a stream is done by using the `/river` endpoint.
Message from this endpoint has the same serialization as the message sent to
`/feeding`, with one addition: an entry query containing the query's name.
River will send messages from all queries associated to the user/token.

Rivers uses Server-sent events to maintain an open connection.

``` shell
curl "http://localhost:4000/river?token=7d0ac81fbdd646dd9e883e3b007ce58d"
data: {"query_id":"dac278b8a6904b469d85df0773d16f5a","body":"{\"doc\":{\"field1\" :\"value1\"}}"}
```

River provides a local cache. If a `last_id` params is given, any item more
recent will be returned.

### Monitoring

Funnel can be monitored on `/status`. Each resquest on this endpoint does a
request on ElasticSearch root.
