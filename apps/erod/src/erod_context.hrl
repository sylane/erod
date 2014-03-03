-ifndef(EROD_CONTEXT_INCLUDED).
-define(EROD_CONTEXT_INCLUDED, true).

-define(Ctx, erod_context).
-record(?Ctx, {log_id :: binary(),
               sess_id :: pos_integer(),
               sess :: pid(),
               user_id :: pos_integer(),
               user :: pid(),
               policy :: erod:policy(),
               actions :: list() | undefined,
               mod :: module(),
               sub :: term()}).


-endif. % EROD_CONTEXT_INCLUDED
