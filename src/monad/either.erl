-module(either).

-behaviour(monad).

-include_lib("eunit/include/eunit.hrl").

-export([
    left/1,
    right/1,
    is_right/1,
    is_left/1
]).

-export([
    map/2,
    flatmap/2,
    cata/3,
    swap/1,
    extract/1
]).

-export_type([
    either/1,
    either/2,
    left/1,
    right/1
]).

-type either(V) :: either(dynamic(), V).
-type either(Err, V) :: left(Err) | right(V).
-type left(X) :: {error, X}.
-type right(X) :: {ok, X}.

left(X) ->
    {error, X}.

right(X) ->
    {ok, X}.

is_right({ok, _}) ->
    true;
is_right(_) ->
    false.

is_left({error, _}) ->
    true;
is_left(_) ->
    false.

map({ok, Value}, F) ->
    {ok, F(Value)};
map(Left = {error, _}, _F) ->
    Left.

flatmap({ok, Value}, F) ->
    case F(Value) of
        Right = {ok, _} ->
            Right;
        Left = {error, _} ->
            Left
    end;
flatmap(Left = {error, _}, _F) ->
    Left.

cata({ok, Value}, _LeftFun, RightFun) ->
    {ok, RightFun(Value)};
cata({error, Value}, LeftFun, _RightFun) ->
    {error, LeftFun(Value)}.

swap({ok, Value}) ->
    {error, Value};
swap({error, Value}) ->
    {ok, Value}.

extract({ok, Value}) ->
    Value;
extract({error, Value}) ->
    Value.

%%%===================================================================
%%% test
%%%===================================================================

flatmap_test_() ->
    [
        {"base test", fun case1/0},
        {"error test", fun case2/0},
        {"dive test", fun case3/0}
    ].

map_test_() ->
    [
        {"base test", fun case4/0},
        {"error test", fun case5/0}
    ].

case1() ->
    DB = #{
        1 => #{name => "a"},
        2 => #{name => "b"}
    },
    PersonWithID = curry:run_curry(curry:curry_right(fun person/2), [2]),

    %% Happy path
    ?assertEqual(
        "b",
        compose:pipe(
            [
                fun(X) -> either:flatmap(X, PersonWithID) end,
                fun(X) -> either:flatmap(X, fun name/1) end,
                fun either:extract/1
            ],
            either:right(DB)
        )
    ).

case2() ->
    DB =
        #{
            1 => #{name => "a"},
            2 => #{name => "b"}
        },
    PersonWithID3 = curry:run_curry(curry:curry_right(fun person/2), [3]),

    ?assertEqual(
        {error, not_found},
        compose:pipe(
            [
                fun(X) -> either:flatmap(X, PersonWithID3) end,
                fun(X) -> either:flatmap(X, fun name/1) end,
                fun either:extract/1
            ],
            either:right(DB)
        )
    ).

case3() ->
    IncFun = fun(X) -> either:right(X + 1) end,

    ?assertEqual(
        11,
        either:extract(
            lists:foldl(
                fun(_, Acc) -> either:flatmap(Acc, IncFun) end,
                either:right(1),
                lists:seq(1, 10)
            )
        )
    ).

case4() ->
    IncFun = fun(X) -> X + 1 end,
    ?assertEqual(
        4,
        compose:pipe(
            [
                fun(X) -> either:map(X, IncFun) end,
                fun(X) -> either:map(X, IncFun) end,
                fun(X) -> either:map(X, IncFun) end,
                fun either:extract/1
            ],
            either:right(1)
        )
    ).

case5() ->
    IncFun = fun(X) -> X + 1 end,
    ?assertEqual(
        1,
        compose:pipe(
            [
                fun(X) -> either:map(X, IncFun) end,
                fun(X) -> either:map(X, IncFun) end,
                fun(X) -> either:map(X, IncFun) end,
                fun either:extract/1
            ],
            either:left(1)
        )
    ).

person(DB, ID) ->
    case maps:get(ID, DB, undefined) of
        undefined ->
            either:left({error, not_found});

        Person ->
            either:right(Person)
    end.

name(Person) ->
    case maps:get(name, Person, undefined) of
        undefined ->
            either:left({error, invalid});

        Name ->
            either:right(Name)
    end.

