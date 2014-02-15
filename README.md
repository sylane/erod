erod
====

Erlang Remote Operative Document.

A framework to build service for clients without reliable connection.
The goal is to keep all the state server-side as a dynamic set of versioned documents.
The clients would retrieve part of these documents and cache them locally like a browser web would do.
The documents can be refreshed from a give version and receive only a patch of the changes.


The first implementation is base on websocket with tight activity monitoring to detect dead connections.
In addition of the login/logout/reconnect operations, the clients use a REST-like set of commands to retrieve and modify part of documents.

Clients can subscribe to some documents to receive change notifications.
