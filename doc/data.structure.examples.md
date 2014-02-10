User
====

Item
----

    {"first_name": "Foo",
     "last_name": "Bar",
     "picture": "user/3243214"
     "presence": "online",
     "connected": true,
     "status": "Spam"}

Content
-------

    []


Favorites/Unite/Department
==========================

Item
----

     {"name": "Some Name"}


Content View "asc_pres_first"
-----------------------------

    [{"key": {"type": "unit", "id": 3412343}, "display_name": "Bar", "picture": "units/3412343"},
     {"key": {"type": "department", "id": 3123321}, "display_name": "Foo", "picture": "departments/3123321"},
     {"key": {"type": "user", "id": 32131}, "display_name": "Abadi Bouzouf", "picture": "users/32131", "presence": "online", "connected": true}
    ]


Conversations
=============

Item
----

    {}


Content View "asc_pres_first"
-----------------------------

    [{"key": {"type": "room", "id": 2314123} "unread": 10, "last_msg": "Hey !",
      "members": [{"key": {"type": "user", "id": 32131}, "display_name": "Abadi Bouzouf", "picture": "users/32131", "presence": "online", "connected": true}]}
    ]


Room
====

Item
----

    {"members": [{"key": {"type": "user", "id": 32131}, "display_name": "Abadi Bouzouf", "picture": "users/32131", "presence": "online", "connected": true}]}

Content
-------

    [{"key": {"type": "msg", "id": [2132332, 434211]}, "time": "20070405T123000", "text": "hi !", read=false}
    ]
