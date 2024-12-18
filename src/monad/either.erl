-module(either).

-behaviour(monad).

-include_lib("eunit/include/eunit.hrl").

-export([
    left/1,
    right/1
]).

%% monad
-export([
    flatmap/2,
    extract/1
]).

-export_type([
    either/0,
    either/1,

    ffun/1,
    ffun/2,
    ffun/0
]).

%% Сюда пишется ошибка, обрывает цепочку исполнения
-record(left, {
    data :: monad:extract_ret()
}).

-record(right, {
    data :: monad:extract_ret()
}).

-type either() :: left() | right().
-type either(X) :: left(X) | right(X).

-type left() :: monad:monad().
-type left(X) :: monad:monad(X).

-type right() :: monad:monad().
-type right(X) :: monad:monad(X).

-type ffun() :: ffun(term(), term()).
-type ffun(X) :: ffun(X, X).
-type ffun(X, Y) :: monad:ffun(X, undefined) | monad:ffun(X, Y).

%%--------------------------------------------------------------------
%% @doc
-spec flatmap(Either, F :: ffun(X, Y)) ->
    Either | either(monad:extract_ret(Y))
when
    Either :: either(monad:extract_ret(X)).
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
-spec left(Data :: monad:extract_ret(X)) ->
    left(X).
%%--------------------------------------------------------------------
left(Data) ->
    #left{data = Data}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec right(Data :: monad:extract_ret(X)) ->
    right(X).
%%--------------------------------------------------------------------
right(Data) ->
    #right{data = Data}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec extract(Either :: either(X)) ->
    monad:extract_ret(X).
%%--------------------------------------------------------------------
extract(Either = #left{}) ->
    Either#left.data;

extract(Either = #right{}) ->
    Either#right.data.
%%--------------------------------------------------------------------

%%%===================================================================
%%% test
%%%===================================================================

base_test() ->
    DB = #{1 => #{name => "a"}, 2 => #{name => "b"}},

    PersonFun = fun person/2,
    NameFun = fun name/1,

    Either = either:right(DB),

    %% Happy path
    Either2 = either:flatmap(either:flatmap(Either, (curry:curry_right(PersonFun))(2)), NameFun),
    ?assertEqual(either:extract(Either2), "b").

error_test() ->
    DB = #{1 => #{name => "a"}, 2 => #{name => "b"}},

    PersonFun = fun person/2,
    NameFun = fun name/1,

    Either = either:right(DB),

    %% Fail
    Either2 = either:flatmap(either:flatmap(Either, (curry:curry_right(PersonFun))(3)), NameFun),
    ?assertEqual(either:extract(Either2), {error, not_found}).

dive_test() ->
    Either = either:right(1),
    IncFun = fun(X) -> either:right(X + 1) end,
    Either2 = (compose:pipe([
        fun(_) ->
            {dive, [(curry:curry_right(fun either:flatmap/2))(IncFun) || _ <- lists:seq(1, 10)]}
        end
    ]))(Either),
    ?assertEqual(either:extract(Either2), 11).

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

