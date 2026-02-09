-module(ok).

-export([ok/1]).

-export_type([ok/1]).

-type ok(X) :: {ok, X}.

ok(X) -> {ok, X}.

