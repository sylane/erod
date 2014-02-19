Login
=====

Request
-------

    {"type": "request",
     "id": "xasdq3213sdfa",
     "cls": "login",
     "data": {"username": "toto",
              "password": "foo"}}


Response
--------

    {"type": "result",
     "id": "xasdq3213sdfa",
     "cls": "login",
     "data": {"session": "asda2ad22da",
              "self": {"type": "user", "id": 423421341},
              "rooms": {"type": "rooms", "id": 232134123},
              "fav": {"type": "fav", "id": 412341235},
              "root": {"type": "index", "id": 234234321},
              "home": {"type": "unit", "id": 234213423},
              "config": {"media_server": "http//foo.bar.com/static"}}}


Error
-----

    {"type": "error",
     "id": "xasdq3213sdfa",
     "cls": "login",
     "data": {"code": 232412}}


Logout
======

Request
-------

    {"type": "request",
     "id": "asdfq3fqaw3aw",
     "cls": "logout"}


Response
--------

    {"type": "result",
     "id": "asdfq3fqaw3aw",
     "cls": "logout"}


Reconnect
=========

Request
-------

    {"type": "request",
     "id": "sdf234fw3ww",
     "cls": "reconnect",
     "data": {"session": "asda2ad22da"}}


Response
--------

    {"type": "result",
     "id": "sdf234fw3ww",
     "cls": "reconnect"}


Content Query
=============

Request
-------

    {"type": "request",
     "id": "asdf323rsasdf",
     "cls": "get_content",
     "data": {"key": {"type": "user", "id": 423421341},
              "ver": null,
              "subscribe": true}}

Not Changed Response
--------------------

    {"type": "result",
     "id": "asdf323rsasdf",
     "cls": "query_content"}


Full Response
-------------

    {"type": "result",
     "id": "asdf323rsasdf",
     "cls": "get_content",
     "data": {"key": {"type": "user", "id": 423421341},
              "ver": ["server_213233213", 123412412],
              "content": {"first_name": "Foo",
                          "last_name": "Bar",
                          "picture": "users/423421341",
                          "presence": "online",
                          "connected": true}}}


Patch response
--------------

    {"type": "result",
     "id": "asdf323rsasdf",
     "cls": "get_content",
     "data": {"key": {"type": "user", "id": 423421341},
              "ver": ["server_213233213", 123412412]
              "patch": [{"op": "replace", "path": "/presence", "offline"}]}}


Page Query
==========

Request
-------

    {"type": "request",
     "id": "qwdf3rs3wr",
     "cls": "get_children",
     "data": {"key": {"type": "fav", "id": 423421341},
              "ver": ["server_213233213", 123413243]
              "view": "asc_pres_first",
              "index": 1,
              "subscribe": true}}


Not Changed Response
--------------------

    {"type": "result",
     "id": "qwdf3rs3wr",
     "cls": "get_children"}


Full Response
-------------

    {"type": "result",
     "id": "qwdf3rs3wr",
     "cls": "get_children",
     "data": {"key": {"type": "fav", "id": 423421341},
              "ver": ["server_213233213", 12341332423]
              "view": "asc_pres_first",
              "index": 1,
              "total_size": 800,
              "page_size": 5,
              "page": [{"key": {"type": "user", "id": 32131}, "display_name": "Abadi Bouzouf", "picture": "users/32131", "presence": "online", "connected": true},
                       {"key": {"type": "user", "id": 87324}, "display_name": "Betani Patif", "picture": "users/87324", "presence": "online", "connected": true},
                       {"key": {"type": "user", "id": 98234}, "display_name": "Coll Derp", "picture": null, "presence": "online", "connected": true},
                       {"key": {"type": "user", "id": 43242}, "display_name": "Delano Manopla", "picture": "users/43242", "presence": "online", "connected": false},
                       {"key": {"type": "user", "id": 21412}, "display_name": "Aneli Patelo", "picture": "users/21412", "presence": "offline", "connected": false}]}}


Patch Response
--------------

    {"type": "result",
     "id": "qwdf3rs3wr",
     "cls": "get_content",
     "data": {"key": {"type": "fav", "id": 423421341},
              "ver": ["server_213233213", 12341332423]
              "view": "asc_pres_first",
              "page": 1,
              "list_size": 800,
              "page_size": 5,
              "patch": [{"op": "add", "path": "/2", "value": {"key": {"type": "user", "id": 98234}, "display_name": "Coll Derp", "picture": null, "presence": "online", "connected": true}},
                        {"op": "remove", "path": "/5"},
                        {"op": "replace", "path": "/3/connected", "value": false}]}}


Modify Content
==============

Request
-------

Updating user status and presence:

    {"type": "request",
     "id": "asdf3wf3r",
     "cls": "patch_content",
     "data": {"key": {"type": "user", "id": 1234234},
              "patch": [{"op": "replace", "path": "/status", "value": "Not Here"},
                        {"op": "replace", "path": "/presence", "value": "busy"}]}},


Sending a read receipt:

    {"type": "request",
     "id": "asdfa3awfs",
     "cls": "patch_content",
     "data": {"key": {"type": "msg", "id": [342131213, 3421321312]},
              "patch": {"op": "add", "path": "/read", "value": true}}}


Success Response
----------------

    {"type": "result",
     "id": "asdf3wf3r",
     "cls": "patch_item",
     "data": {"ver": ["server_213233213", 23479087213]}}


Error Response
--------------

    {"type": "error",
     "id": "asdf3wf3r",
     "cls": "patch_item",
     "data": {"code": 10323}}


Adding Children
===============

Request
-------

Creating a chat session:

    {"type": "request",
     "id": "sdf3223dqs",
     "cls": "add_child",
     "data": {"key": {"type": "conversations", "id": 3421321312},
              "obj": {"key": {"type": "room"}, "members": [{"key": {"type": "user", "id": 123213}}]}}}

Sending a message:

    {"type": "request",
     "id": "sdf3234",
     "cls": "add_child",
     "data": {"key": {"type": "room", "id": 3421321312},
              "obj": {"key": {"type": "msg"}, "text": "hi!"}}}


Response
--------

    {"type": "result",
     "id": "sdf3223dqs",
     "cls": "add_child",
     "data": {"key": {"type": "room", "id": 2134123}}}



Subscribe
=========

    {"type": "notify",
     "cls": "subscribe",
     "data": [{"type": "user", "id": 231213123},
              {"type": "unit", "id": 4324123}]}


Revoke Subscriptions
====================

    {"type": "notify",
     "cls": "revoke",
     "data": [{"type": "user", "id": 231213123},
              {"type": "unit", "id": 4324123}]}


Change Notification
===================

Request
-------

    {"type": "request",
     "id": "wdfqwef32f23f",
     "cls": "changed",
     "data": [{"key": {"type": "user", "id": 231213123}, "content": true, "children": false},
              {"key": {"type": "unit", "id": 4324123}, "content": true, "children": false},
              {"key": {"type": "fav", "id": 4312341233}, "content": false, "children": true},
              {"key": {"type": "unit", "id": 3512232}, "content": true, "children": true}]}

Response
--------

    {"type": "result",
     "id": "wdfqwef32f23f",
     "cls": "changed"}
