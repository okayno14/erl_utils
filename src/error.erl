-module(error).

-export([
    error/1,
    error/2
]).

-export_type([
    error/0, error/1, error/2,
    reason/0, reason/1,
    reason_msg/0, reason_msg/2,
    reason_msg_st/0, reason_msg_st/3
]).

-define(is_error(X),
    element(1, X) == error andalso (tuple_size(X) == 2 orelse tuple_size(X) == 3)
).

-type error() ::
    {error, error_body()}
    | {error, error_body(), error()}.
-type error(ErrorBody) :: {error, ErrorBody}.
-type error(ErrorBody, Error) :: {error, ErrorBody, Error}.

-type error_body() ::
    reason() |
    reason_msg() |
    reason_msg_st().

-type reason() :: atom().
-type reason(X) :: X.

-type reason_msg() :: {Reason :: atom(), Msg :: unicode:chardata()}.
-type reason_msg(Reason, Msg) :: {Reason, Msg}.

-type reason_msg_st() :: {Reason :: atom(), Msg :: unicode:chardata(), Stacktrace :: erlang:stacktrace()}.
-type reason_msg_st(Reason, Msg, Stacktrace) :: {Reason, Msg, Stacktrace}.

%%--------------------------------------------------------------------
-spec error
    (reason(Reason :: atom())) -> error(Reason :: atom());
    (reason_msg(Reason :: atom(), Msg :: unicode:chardata())) ->
        error(reason_msg(Reason :: atom(), Msg :: unicode:chardata()));
    (
        reason_msg_st(
            Reason :: atom(),
            Msg :: unicode:chardata(),
            Stacktrace :: erlang:stacktrace()
        )
    ) ->
        error(
            reason_msg_st(
                Reason :: atom(),
                Msg :: unicode:chardata(),
                Stacktrace :: erlang:stacktrace()
            )
        ).
%%--------------------------------------------------------------------
error(Reason) when
    is_atom(Reason)
->
    {error, Reason};
error({Reason, Msg}) when
    is_atom(Reason),
    (is_list(Msg) or is_binary(Msg))
->
    {error, {Reason, Msg}};
error({Reason, Msg, Stacktrace}) when
    is_atom(Reason),
    (is_list(Msg) or is_binary(Msg)),
    is_list(Stacktrace)
->
    {error, {Reason, Msg, Stacktrace}}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec error
    (reason(Reason :: atom()), error()) -> error(Reason :: atom(), error());
    (reason_msg(Reason :: atom(), Msg :: unicode:chardata()), error()) ->
        error(reason_msg(Reason :: atom(), Msg :: unicode:chardata()), error());
    (
        reason_msg_st(
            Reason :: atom(),
            Msg :: unicode:chardata(),
            Stacktrace :: erlang:stacktrace()
        ),
        error()
    ) ->
        error(
            reason_msg_st(
                Reason :: atom(),
                Msg :: unicode:chardata(),
                Stacktrace :: erlang:stacktrace()
            ),
            error()
        ).
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

