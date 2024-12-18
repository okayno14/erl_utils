-module(maybe).

-behaviour(monad).

-include_lib("eunit/include/eunit.hrl").

-export([
    maybe/1
]).

%% monad
-export([
    flatmap/2,
    extract/1
]).

-export_type([
    maybe/0,
    maybe/1,

    ffun/1,
    ffun/2,
    ffun/0
]).

-record(maybe, {
    data :: undefined | monad:extract_ret()
}).

-type maybe() :: monad:monad().
-type maybe(X) :: monad:monad(X).

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
flatmap(Maybe = #maybe{}, F) ->
    case extract(Maybe) of
        undefined ->
            Maybe;

        _ ->
            F(extract(Maybe))
    end.
%%--------------------------------------------------------------------

%%%===================================================================
%%% maybe-object
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec maybe(Data) ->
    maybe(Data).
%%--------------------------------------------------------------------
maybe(Data) ->
    #maybe{data = Data}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec extract(Maybe :: maybe(X)) ->
    monad:extract_ret(X | undefined).
%%--------------------------------------------------------------------
extract(Maybe = #maybe{}) ->
    Maybe#maybe.data.
%%--------------------------------------------------------------------

%%%===================================================================
%%% test
%%%===================================================================

base_test() ->
    Maybe = maybe:maybe(1),
    IncFun = inc_fun(),
    Maybe2 = maybe:flatmap(maybe:flatmap(maybe:flatmap(Maybe, IncFun), IncFun), IncFun),
    ?assertEqual(maybe:extract(Maybe2), 4).

pipe_test() ->
    Maybe = maybe:maybe(1),
    IncFun = inc_fun(),
    Maybe2 = (compose:pipe([
        (curry:curry_right(fun maybe:flatmap/2))(IncFun),
        (curry:curry_right(fun maybe:flatmap/2))(IncFun),
        (curry:curry_right(fun maybe:flatmap/2))(IncFun)
    ]))(Maybe),
    ?assertEqual(maybe:extract(Maybe2), 4).

undefined_test() ->
    Maybe = maybe:maybe(1),
    IncFun = inc_fun(),
    Maybe2 = maybe:flatmap(maybe:flatmap(maybe:flatmap(Maybe, IncFun), fun(_) -> maybe:maybe(undefined) end), IncFun),
    ?assertEqual(maybe:extract(Maybe2), undefined).

dive_test() ->
    Maybe = maybe:maybe(1),
    IncFun = inc_fun(),
    Maybe2 = (compose:pipe([
        fun(_) -> {dive, [(curry:curry_right(fun maybe:flatmap/2))(IncFun) || _ <- lists:seq(1, 10)]} end
    ]))(Maybe),
    ?assertEqual(maybe:extract(Maybe2), 11).

inc_fun() ->
    fun(X) -> maybe:maybe(X + 1) end.

