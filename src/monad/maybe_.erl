-module(maybe_).

-behaviour(monad).

-include_lib("eunit/include/eunit.hrl").

-export([
    value/1,
    empty/0,
    is_value/1,
    is_empty/1,
    extract/2
]).

%% monad
-export([
    map/2,
    flatmap/2,
    extract/1
]).

-export_type([
    maybe_/1,
    value/1
]).

-record(value, {data :: term()}).
-record(empty, {data = undefined :: undefined}).

-opaque maybe_(X) :: value(X) | empty().
-type value(X) :: #value{data :: X}.
-type empty() :: #empty{}.

%%--------------------------------------------------------------------
%% @doc
-spec map(Maybe, F :: monad:map_fun(X, Y)) ->
    Maybe | Maybe2
when
    Maybe :: maybe_(X),
    Maybe2 :: maybe_(Y).
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
-spec flatmap(Maybe, F :: fun((X) -> maybe_(Y))) ->
    Maybe | Maybe2
when
    Maybe :: maybe_(X),
    Maybe2 :: maybe_(Y).
%%--------------------------------------------------------------------
flatmap(Value = #value{}, F) ->
    #value{data = Data} = Value,
    F(Data);

flatmap(Empty = #empty{}, _F) ->
    Empty.
%%--------------------------------------------------------------------

%%%===================================================================
%%% maybe_-object
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
%% @doc Ожидает value, иначе - error:function_clause
-spec extract(Maybe :: value(X)) ->
    X.
%%--------------------------------------------------------------------
extract(Value = #value{}) ->
    Value#value.data.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Вернёт Default в случае empty()
-spec extract(Maybe :: maybe_(X), Default) ->
    X | Default.
%%--------------------------------------------------------------------
extract(Value = #value{}, _Default) ->
    Value#value.data;
extract(#empty{}, Default) ->
    Default.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec is_value(Maybe :: maybe_(_X)) ->
    boolean().
%%--------------------------------------------------------------------
is_value(#value{}) ->
    true;
is_value(#empty{}) ->
    false.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec is_empty(Maybe :: maybe_(_X)) ->
    boolean().
%%--------------------------------------------------------------------
is_empty(#empty{}) ->
    true;
is_empty(#value{}) ->
    false.
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
    Maybe = maybe_:value(1),
    IncFun = inc_flatmap_fun(),
    Maybe2 = maybe_:flatmap(maybe_:flatmap(maybe_:flatmap(Maybe, IncFun), IncFun), IncFun),
    ?assertEqual(maybe_:extract(Maybe2), 4).

case2() ->
    Maybe = maybe_:value(1),
    IncFun = inc_flatmap_fun(),

    ?assertEqual(
        4,
        maybe_:extract(
            compose:pipe(
                [
                    (curry:curry_right(fun maybe_:flatmap/2))(IncFun),
                    (curry:curry_right(fun maybe_:flatmap/2))(IncFun),
                    (curry:curry_right(fun maybe_:flatmap/2))(IncFun)
                ],
                Maybe
            )
        )
    ).

case3() ->
    Maybe = maybe_:value(1),
    IncFun = inc_flatmap_fun(),
    Maybe2 = maybe_:flatmap(maybe_:flatmap(maybe_:flatmap(Maybe, IncFun), fun(_) -> maybe_:empty() end), IncFun),
    ?assertEqual(maybe_:extract(Maybe2, undefined), undefined).

case5() ->
    Maybe = maybe_:value(1),
    IncFun = inc_map_fun(),
    Maybe2 = maybe_:map(maybe_:map(maybe_:map(Maybe, IncFun), IncFun), IncFun),
    ?assertEqual(maybe_:extract(Maybe2), 4).

case6() ->
    Maybe = maybe_:value(1),
    IncFun = inc_map_fun(),

    ?assertEqual(
        4,
        maybe_:extract(
            compose:pipe(
                [
                    (curry:curry_right(fun maybe_:map/2))(IncFun),
                    (curry:curry_right(fun maybe_:map/2))(IncFun),
                    (curry:curry_right(fun maybe_:map/2))(IncFun)
                ],
                Maybe
            )
        )
    ).

case7() ->
    Maybe = maybe_:value(1),
    Maybe2 =
    maybe_:map(
        maybe_:map(
            Maybe,
            fun(X) -> X + 1 end
        ),
        fun
            (1) -> "a";
            (2) -> "b";
            (3) -> "c"
        end
    ),
    ?assertEqual("b", maybe_:extract(Maybe2)).

inc_map_fun() ->
    fun(X) -> X + 1 end.

inc_flatmap_fun() ->
    fun(X) -> maybe_:value(X + 1) end.

