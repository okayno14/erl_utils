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
    maybe/1,
    value/1
]).

-record(value, {data :: term()}).
-record(empty, {data = undefined :: undefined}).

-opaque maybe(X) :: value(X) | empty().
-type value(X) :: #value{data :: X}.
-type empty() :: #empty{}.

%%--------------------------------------------------------------------
%% @doc
-spec map(Maybe, F :: monad:map_fun(X, Y)) ->
    Maybe | Maybe2
when
    Maybe :: maybe(X),
    Maybe2 :: maybe(Y).
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
-spec flatmap(Maybe, F :: fun((X) -> maybe(Y))) ->
    Maybe | Maybe2
when
    Maybe :: maybe(X),
    Maybe2 :: maybe(Y).
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
-spec value(Data :: X) ->
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
    undefined | X.
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
        {"undefined_test", fun case3/0}
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

    Maybe2 =
    compose:pipe(Maybe, [
        (curry:curry_right(fun maybe:flatmap/2))(IncFun),
        (curry:curry_right(fun maybe:flatmap/2))(IncFun),
        (curry:curry_right(fun maybe:flatmap/2))(IncFun)

    ]),

    ?assertEqual(maybe:extract(Maybe2), 4).

case3() ->
    Maybe = maybe:value(1),
    IncFun = inc_flatmap_fun(),
    Maybe2 = maybe:flatmap(maybe:flatmap(maybe:flatmap(Maybe, IncFun), fun(_) -> maybe:empty() end), IncFun),
    ?assertEqual(maybe:extract(Maybe2), undefined).

case5() ->
    Maybe = maybe:value(1),
    IncFun = inc_map_fun(),
    Maybe2 = maybe:map(maybe:map(maybe:map(Maybe, IncFun), IncFun), IncFun),
    ?assertEqual(maybe:extract(Maybe2), 4).

case6() ->
    Maybe = maybe:value(1),
    IncFun = inc_map_fun(),

    Maybe2 =
    compose:pipe(Maybe, [
        (curry:curry_right(fun maybe:map/2))(IncFun),
        (curry:curry_right(fun maybe:map/2))(IncFun),
        (curry:curry_right(fun maybe:map/2))(IncFun)
    ]),

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

