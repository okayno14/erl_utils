-module(maybe).

-behaviour(monad).

-include_lib("eunit/include/eunit.hrl").

-export([
    value/1,
    empty/0
]).

%% monad
-export([
    flatmap/2,
    extract/1
]).

-export_type([
    maybe/0,
    maybe/1,

    value/0,
    value/1,

    empty/0,

    ffun/1,
    ffun/2,
    ffun/0
]).

-record(value, {data :: monad:extract_ret()}).
-record(empty, {data = undefined}).

-type maybe() :: value() | empty().
-type maybe(X) :: value(X).

-type value() :: #value{}.
-type value(X) :: monad:monad(X).

-type empty() :: #empty{}.

-type ffun() :: ffun(term(), term()).
-type ffun(X) :: ffun(X, X).
-type ffun(X, Y) :: monad:ffun(X, undefined) | monad:ffun(X, Y).

%%--------------------------------------------------------------------
%% @doc
-spec flatmap(Maybe, F :: ffun(X, Y)) ->
    Maybe | maybe(monad:extract_ret(Y))
when
    Maybe :: maybe(monad:extract_ret(X)).
%%--------------------------------------------------------------------
flatmap(Value = #value{}, F) ->
    #value{data = Data} = Value,
    F(Data);

flatmap(Empty = #empty{}, _F) ->
    Empty.
%%--------------------------------------------------------------------

%%%===================================================================
%%% maybe-object
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec value(Data :: monad:extract_ret(X)) ->
    value(X).
%%--------------------------------------------------------------------
value(Data) ->
    #value{data = Data}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec empty() ->
    empty().
%%--------------------------------------------------------------------
empty() ->
    #empty{}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec extract(Maybe :: maybe(X)) ->
    monad:extract_ret(X) | undefined.
%%--------------------------------------------------------------------
extract(Value = #value{}) ->
    Value#value.data;

extract(Empty = #empty{}) ->
    Empty#empty.data.
%%--------------------------------------------------------------------

%%%===================================================================
%%% test
%%%===================================================================

base_test() ->
    Maybe = maybe:value(1),
    IncFun = inc_fun(),
    Maybe2 = maybe:flatmap(maybe:flatmap(maybe:flatmap(Maybe, IncFun), IncFun), IncFun),
    ?assertEqual(maybe:extract(Maybe2), 4).

pipe_test() ->
    Maybe = maybe:value(1),
    IncFun = inc_fun(),
    Maybe2 = (compose:pipe([
        (curry:curry_right(fun maybe:flatmap/2))(IncFun),
        (curry:curry_right(fun maybe:flatmap/2))(IncFun),
        (curry:curry_right(fun maybe:flatmap/2))(IncFun)
    ]))(Maybe),
    ?assertEqual(maybe:extract(Maybe2), 4).

undefined_test() ->
    Maybe = maybe:value(1),
    IncFun = inc_fun(),
    Maybe2 = maybe:flatmap(maybe:flatmap(maybe:flatmap(Maybe, IncFun), fun(_) -> maybe:empty() end), IncFun),
    ?assertEqual(maybe:extract(Maybe2), undefined).

dive_test() ->
    Maybe = maybe:value(1),
    IncFun = inc_fun(),
    Maybe2 = (compose:pipe([
        fun(_) -> {dive, [(curry:curry_right(fun maybe:flatmap/2))(IncFun) || _ <- lists:seq(1, 10)]} end
    ]))(Maybe),
    ?assertEqual(maybe:extract(Maybe2), 11).

inc_fun() ->
    fun(X) -> maybe:value(X + 1) end.

