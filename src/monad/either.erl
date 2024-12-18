-module(either).

-behaviour(monad).

-include_lib("eunit/include/eunit.hrl").

-export([
    pipe/2,
    left/1,
    right/1
]).

%% monad
-export([
    bind/2,
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
-spec pipe(Either :: either(), ListFun :: [ffun()]) ->
    either().
%%--------------------------------------------------------------------
pipe(Either, ListFun) ->
    monad:pipe(?MODULE, Either, ListFun).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec bind(Either, F :: ffun(X, Y)) ->
    Either | either(monad:extract_ret(Y))
when
    Either :: either(monad:extract_ret(X)).
%%--------------------------------------------------------------------
bind(Left = #left{}, _F) ->
    Left;

bind(Right = #right{}, F) ->
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

    Either = right(DB),

    %% Happy path
    Either2 = bind(bind(Either, (curry:curry_right(PersonFun))(2)), NameFun),
    ?assertEqual(extract(Either2), "b").

error_test() ->
    DB = #{1 => #{name => "a"}, 2 => #{name => "b"}},

    PersonFun = fun person/2,
    NameFun = fun name/1,

    Either = right(DB),

    %% Fail
    Either2 = bind(bind(Either, (curry:curry_right(PersonFun))(3)), NameFun),
    ?assertEqual(extract(Either2), {error, not_found}).

dive_test() ->
    Either = right(1),
    IncFun = fun(X) -> right(X + 1) end,
    Either2 =
    pipe(Either, [
        fun(X) -> right({dive, right(X), [IncFun || _ <- lists:seq(1, 10)]}) end
    ]),
    ?assertEqual(extract(Either2), 11).

person(DB, ID) ->
    case maps:get(ID, DB, undefined) of
        undefined ->
            left({error, not_found});

        Person ->
            right(Person)
    end.

name(Person) ->
    case maps:get(name, Person, undefined) of
        undefined ->
            left({error, invalid});

        Name ->
            right(Name)
    end.

