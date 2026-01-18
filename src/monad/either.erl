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

-type either(V) :: either(V, term()).
-type either(V, Err) :: left(V) | right(Err).
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
    DB = #{1 => #{name => "a"}, 2 => #{name => "b"}},

    PersonFun = fun person/2,
    NameFun = fun name/1,

    Either = either:right(DB),

    %% Happy path
    Either2 = either:flatmap(either:flatmap(Either, (curry:curry_right(PersonFun))(2)), NameFun),
    ?assertEqual(either:extract(Either2), "b").

case2() ->
    DB = #{1 => #{name => "a"}, 2 => #{name => "b"}},

    PersonFun = fun person/2,
    NameFun = fun name/1,

    Either = either:right(DB),

    %% Fail
    Either2 = either:flatmap(either:flatmap(Either, (curry:curry_right(PersonFun))(3)), NameFun),
    ?assertEqual(either:extract(Either2), {error, not_found}).

case3() ->
    Either = either:right(1),
    IncFun = fun(X) -> either:right(X + 1) end,

    ?assertEqual(
        11,
        either:extract(
            compose:pipe(
                [(curry:curry_right(fun either:flatmap/2))(IncFun) || _ <- lists:seq(1, 10)],
                Either
            )
        )
    ).

case4() ->
    Either = either:right(1),
    IncFun = fun(X) -> X + 1 end,
    Either2 = either:map(either:map(either:map(Either, IncFun), IncFun), IncFun),
    ?assertEqual(4, either:extract(Either2)).

case5() ->
    Either = either:left(1),
    IncFun = fun(X) -> X + 1 end,
    Either2 = either:map(either:map(either:map(Either, IncFun), IncFun), IncFun),
    ?assertEqual(1, either:extract(Either2)).

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

