# Architecture

funnel is meant to receive content and notify when relevant new content is
available.


  /-----------\   /-----------\
  |  Source1  |   | Source 2  |
  \-----------/   \-----------/
         |              |
         :              :
         :              :
         V              V
     /-----------------------\      /-----------------\
     |                       |<---->|{s}              |
     |         funnel        |      |  ElasticSearch  |
     |                       |      |                 |
     \-----------------------/      \-----------------/
                :   :
                :   :
                V   V
     /------------+-----------\
     | Consumer   | Consumer  |
     \------------+-----------/

## API

funnel exposes a json API to communicate the the world.


### Adding messages

Adding messages is done by using the `/feeding` endpoint. The payload must
comply with the funnel's message serialization. This entries can accept a single
message, or a list of message.


### Register

A user, or a device, can register to funnel by using the `/ohai` endpoint.
This will returns a token. This token must be used in all communication with the
funnel's API.


### Adding filters

Adding filters is done by using the `/filter` endpoint. The payload must
comply with the funnel's filter serialization. This entries can accept a single
filter, or a list of filters.

A filter is defined by a user's token, a name, and a json representing the
elasticsearch query.


### Stream

Listening to a stream is done by using the `/river` endpoint.
Message from this endpoint has the same serialization as the message sended to
`/feeding`, with one addition: an entry filter containing the filter's name.
River will send messages from all filters associated to the user/token.

Rivers uses Server-sent events to maintain a open connexion.


## Common workflow

### Client point of view

  * send data to `/feeding`
  * register on the `/ohai` endpoint
  * receive a token
  * add filter to `/filter`
  * listen to `/river`


### Funnel point of view

  * receive data on `/feeding`
  * check this data to Elasticseach percolator
  * forward message to client for each matching filter


## Storage

### Messages

Message are not persisted by funnel. Period.

River does have a local cache to support `last-event-id` from Server-sent
events.

Although, a webservice could register on funnel, and persist relevant messages.


### Filters

For now, directly into Elasticseach. Not really its job, but, it will allow to
have only one dependency.
