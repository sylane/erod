erod
====

Erlang Remote Operative Documents.

!!! THIS PROJECT IS STILL IN PROTOTYPING PHASE !!!

It is a framework to build services for clients without reliable connection.

The goal is to keep all the service state server-side as a dynamic set
of versioned documents.

Each documents have a unique key and they can interact with each others.
They can request to be notified of other documents changes
to stay synchronized and send events to other documents or do whatever
a Erlang process could do.

Each document have a structured content and a list of children.
Each child represent a summary of another document and contains its key.
A document can maintain multiple children views each with its own
ordering and summary data structure.

The clients can request a document content or a page of children for
a specific view, and will receive the requested entity and its version identifier.
If the client specifies a local version identifier, the server may return a patch
instead of a full entity with a new version; this patch applied
to the local entity will update it the the new version.

When retrieving entities, the client can subscribe to the document.
When subscribed, the client receives notifications that some documents content
or children changed. The notification only contains the documents key, the client
is free to update all or part of the changed documents as needed.
In addition, the client can subscribe and unsubscribe to any document
directly without having to retrieve anything.

The notifications are sent periodically and by consolidated block to prevent
notification flood. If a document changes multiple times in quick successions,
the client receives only one reference to it in its next notification block.

The client confirms the reception of the notification blocks to prevent any
temporary network outage to flood the client when recovering connectivity.

The clients may cache these document parts locally like a browser web,
and refresh them asynchronously as the user is browsing the data;
it may do some prefetching to optimize the rendering.

To interact with these document the client can send a patch for a specific
document content; the server-side document's logic validates the changes,
updates the server-side state, propagates it to other document if relevant and
returns the new content version.

Another way for the client to modify the server state, is to add a child
to a document. The server-side document's logic validates the new child data,
adds any optional values, maybe creates a new server-side document for it
and finally adds it to the targeted document.

The current implementation uses websockets with tight activity monitoring to detect dead connections.
In addition to the login/logout/reconnect operations, the clients can use a REST-like set of commands
to retrieve and modify part of documents as explained before.
