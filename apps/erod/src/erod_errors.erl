-module(erod_errors).

-export([map/1]).


map(Error) -> {500, <<"Internal Server Error">>, Error}.


