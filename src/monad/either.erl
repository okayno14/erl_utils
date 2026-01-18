-module(either).

-behaviour(monad).

-include_lib("eunit/include/eunit.hrl").

-export([
    left/1,
    right/1,
    is_right/1,
    is_left/1
]).

%% monad
-export([
    map/2,
    flatmap/2,
    extract/1
]).

-export_type([
    either/1,
    left/1,
    right/1
]).

%% Сюда пишется ошибка, обрывает цепочку исполнения
-record(left, {
    data :: term()
}).

-record(right, {
    data :: term()
}).

-opaque either(X) :: left(X) | right(X).
-opaque left(X) :: #left{data :: X}.
-opaque right(X) :: #right{data :: X}.

%%--------------------------------------------------------------------
%% @doc
-spec map(Either, F :: monad:map_fun(X, Y)) ->
    Either | Either2
when
    Either :: either(X),
    Either2 :: either(Y).
%%--------------------------------------------------------------------
map(Left = #left{}, _F) ->
    Left;

map(Right = #right{}, F) ->
    right(F(extract(Right))).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec flatmap(Either, F :: monad:flatmap_fun(X, Y)) ->
    Either | Either2
when
    Either :: either(X),
    Either2 :: either(Y).
%%--------------------------------------------------------------------
flatmap(Left = #left{}, _F) ->
    Left;

flatmap(Right = #right{}, F) ->
    F(extract(Right)).
%%--------------------------------------------------------------------

%%%===================================================================
%%% either-object
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec left(Data :: X) ->
    left(X).
%%--------------------------------------------------------------------
left(Data) ->
    #left{data = Data}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec right(Data :: X) ->
    right(X).
%%--------------------------------------------------------------------
right(Data) ->
    #right{data = Data}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec is_right(Either :: either(_)) ->
    boolean().
%%--------------------------------------------------------------------
is_right(#right{}) ->
    true;
is_right(#left{}) ->
    false.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec is_left(Either :: either(_)) ->
    boolean().
%%--------------------------------------------------------------------
is_left(#left{}) ->
    true;
is_left(#right{}) ->
    false.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec extract(Either :: either(X)) ->
    X.
%%--------------------------------------------------------------------
extract(Either = #left{}) ->
    Either#left.data;

extract(Either = #right{}) ->
    Either#right.data.
%%--------------------------------------------------------------------

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

