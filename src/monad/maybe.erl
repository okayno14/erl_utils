-module(maybe).

-behaviour(monad).

-include_lib("eunit/include/eunit.hrl").

-export([
    value/1,
    empty/0
]).

%% monad
-export([
    map/2,
    flatmap/2,
    extract/1
]).

-export_type([
    maybe/0,
    maybe/1,

    value/0,
    value/1,

    empty/0
]).

-record(value, {data :: monad:extract_ret()}).
-record(empty, {data = undefined}).

-type maybe() :: value() | empty().
-type maybe(X) :: value(X).

-type value() :: #value{}.
-type value(X) :: monad:monad(X).

-type empty() :: #empty{}.

%%--------------------------------------------------------------------
%% @doc
-spec map(Maybe, F :: monad:map_fun(X, Y)) ->
    Maybe | maybe(monad:extract_ret(Y))
when
    Maybe :: maybe(monad:extract_ret(X)).
%%--------------------------------------------------------------------
%% TODO переделать на функциональное апи сущности
map(Value = #value{}, F) ->
    #value{data = Data} = Value,
    value(F(Data));

map(Empty = #empty{}, _F) ->
    Empty.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec flatmap(Maybe, F :: monad:flatmap_fun(X, Y)) ->
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

flatmap_test_() ->
    [
        {"base case", fun case1/0},
        {"pipe_test", fun case2/0},
        {"undefined_test", fun case3/0},
        {"dive_test", fun case4/0}
    ].

map_test_() ->
    [
        {"base case", fun case5/0},
        {"pipe_test", fun case6/0},
        {"type conversion", fun case7/0}
    ].

case1() ->
    Maybe = maybe:value(1),
    IncFun = inc_flatmap_fun(),
    Maybe2 = maybe:flatmap(maybe:flatmap(maybe:flatmap(Maybe, IncFun), IncFun), IncFun),
    ?assertEqual(maybe:extract(Maybe2), 4).

case2() ->
    Maybe = maybe:value(1),
    IncFun = inc_flatmap_fun(),
    Maybe2 = (compose:pipe([
        (curry:curry_right(fun maybe:flatmap/2))(IncFun),
        (curry:curry_right(fun maybe:flatmap/2))(IncFun),
        (curry:curry_right(fun maybe:flatmap/2))(IncFun)
    ]))(Maybe),
    ?assertEqual(maybe:extract(Maybe2), 4).

case3() ->
    Maybe = maybe:value(1),
    IncFun = inc_flatmap_fun(),
    Maybe2 = maybe:flatmap(maybe:flatmap(maybe:flatmap(Maybe, IncFun), fun(_) -> maybe:empty() end), IncFun),
    ?assertEqual(maybe:extract(Maybe2), undefined).

case4() ->
    Maybe = maybe:value(1),
    IncFun = inc_flatmap_fun(),
    Maybe2 = (compose:pipe([
        fun(_) -> {dive, [(curry:curry_right(fun maybe:flatmap/2))(IncFun) || _ <- lists:seq(1, 10)]} end
    ]))(Maybe),
    ?assertEqual(maybe:extract(Maybe2), 11).

case5() ->
    Maybe = maybe:value(1),
    IncFun = inc_map_fun(),
    Maybe2 = maybe:map(maybe:map(maybe:map(Maybe, IncFun), IncFun), IncFun),
    ?assertEqual(maybe:extract(Maybe2), 4).

case6() ->
    Maybe = maybe:value(1),
    IncFun = inc_map_fun(),
    Maybe2 = (compose:pipe([
        (curry:curry_right(fun maybe:map/2))(IncFun),
        (curry:curry_right(fun maybe:map/2))(IncFun),
        (curry:curry_right(fun maybe:map/2))(IncFun)
    ]))(Maybe),
    ?assertEqual(maybe:extract(Maybe2), 4).

case7() ->
    Maybe = maybe:value(1),
    Maybe2 =
    maybe:map(
        maybe:map(
            Maybe,
            fun(X) -> X + 1 end
        ),
        fun
            (1) -> "a";
            (2) -> "b";
            (3) -> "c"
        end
    ),
    ?assertEqual("b", maybe:extract(Maybe2)).

inc_map_fun() ->
    fun(X) -> X + 1 end.

inc_flatmap_fun() ->
    fun(X) -> maybe:value(X + 1) end.

