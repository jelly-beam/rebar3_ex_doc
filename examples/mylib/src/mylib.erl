-module(mylib).

-export([a_function/0]).

% @doc a is Ok!
-spec a_function() -> ok.
a_function() ->
  ok.
