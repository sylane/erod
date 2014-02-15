erod
====

Erlang Remote Operative Documents.

A framework to build services for clients without reliable connection.
The goal is to keep all the state server-side as a dynamic set of versioned documents.
The clients would retrieve part of these documents and cache them locally like a browser web would do.
The documents can be refreshed from a given version, receiving only a patch of the changes.
The clients can subscribe to some documents to receive change notifications.
In addition of the login/logout/reconnect operations, the clients can use a REST-like set of commands to retrieve and modify part of documents.

The current implementation uses websockets with tight activity monitoring to detect dead connections.
