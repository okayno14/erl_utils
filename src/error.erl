-module(error).

-export([
    error/1,
    error/2
]).

-export_type([
    error/0
]).

-define(is_error(X),
    element(1, X) == error andalso (tuple_size(X) == 2 orelse tuple_size(X) == 3)
).

-type error() ::
    {error, error_body()}
    | {error, error_body(), error()}.

-type error_body() ::
    Reason ::
    atom()
    | {
        Reason :: atom(),
        Msg :: unicode:chardata()
    }
    | {
        Reason :: atom(),
        Msg :: unicode:chardata(),
        Stacktrace :: erlang:stacktrace()
    }.

%%--------------------------------------------------------------------
-spec error(Reason :: error_body()) ->
    error().
%%--------------------------------------------------------------------
error(Reason)
when
    is_atom(Reason)
->
    {error, Reason};
error({Reason, Msg})
when
    is_atom(Reason),
    (is_list(Msg) or is_binary(Msg))
->
    {error, {Reason, Msg}};
error({Reason, Msg, Stacktrace})
when
    is_atom(Reason),
    (is_list(Msg) or is_binary(Msg)),
    is_list(Stacktrace)
->
    {error, {Reason, Msg, Stacktrace}}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec error(Reason :: error_body(), Error :: error()) ->
    error().
%%--------------------------------------------------------------------
error(Reason, Error)
when
    is_atom(Reason),
    ?is_error(Error)
->
    {error, Reason, Error};
error({Reason, Msg}, Error)
when
    is_atom(Reason),
    (is_list(Msg) or is_binary(Msg)),
    ?is_error(Error)
->
    {error, {Reason, Msg}, Error};
error({Reason, Msg, Stacktrace}, Error)
when
    is_atom(Reason),
    (is_list(Msg) or is_binary(Msg)),
    is_list(Stacktrace),
    ?is_error(Error)
->
    {error, {Reason, Msg, Stacktrace}, Error}.
%%--------------------------------------------------------------------

