-include_lib("erod/include/erod_document.hrl").

-record(erdom_index, {group_ids}).

-record(erdom_group, {id, name, user_ids}).

-record(erdom_user, {id, first_name, last_name, display_name}).



-record(erdom_index_content, {}).

-record(erdom_index_child, {name}).

-record(erdom_group_content, {name}).

-record(erdom_group_child, {name, presence, connected}).

-record(erdom_user_content, {first_name, last_name,
                             display_name, picture,
                             presence, connected, status}).
