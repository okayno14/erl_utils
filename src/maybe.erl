-module(maybe).

-behaviour(monad).

-include_lib("eunit/include/eunit.hrl").

-export([
    pipe/2,
    maybe/1
]).

%% monad
-export([
    bind/2,
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
    data :: undefined | {dive, Maybe :: maybe(), L :: [ffun()]} | term()
}).

-type maybe() :: monad:monad().
-type maybe(X) :: monad:monad(X).

-type ffun() :: ffun(term(), term()).
-type ffun(X) :: ffun(X, X).
-type ffun(X, Y) :: monad:ffun(X, undefined) | monad:ffun(X, Y).

%%--------------------------------------------------------------------
%% @doc
-spec pipe(Maybe :: maybe(), ListFun :: [ffun()]) ->
    maybe().
%%--------------------------------------------------------------------
pipe(Maybe, ListFun) ->
    monad:pipe(?MODULE, Maybe, ListFun).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec bind(Maybe, F :: ffun(X, Y)) ->
    Maybe | maybe(monad:extract_ret(Y))
when
    Maybe :: maybe(monad:extract_ret(X)).
%%--------------------------------------------------------------------
bind(Maybe = #maybe{}, F) ->
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
    Maybe = maybe(1),
    IncFun = inc_fun(),
    Maybe2 = bind(bind(bind(Maybe, IncFun), IncFun), IncFun),
    ?assertEqual(extract(Maybe2), 4).

pipe_test() ->
    Maybe = maybe(1),
    IncFun = inc_fun(),
    Maybe2 = pipe(Maybe, [IncFun, IncFun, IncFun]),
    ?assertEqual(extract(Maybe2), 4).

undefined_test() ->
    Maybe = maybe(1),
    IncFun = inc_fun(),
    Maybe2 = bind(bind(bind(Maybe, IncFun), fun(_) -> maybe(undefined) end), IncFun),
    ?assertEqual(extract(Maybe2), undefined).

dive_test() ->
    Maybe = maybe(1),
    IncFun = inc_fun(),
    Maybe2 =
    pipe(Maybe, [
        fun(X) -> maybe({dive, maybe(X), [IncFun || _ <- lists:seq(1, 10)]}) end
    ]),
    ?assertEqual(extract(Maybe2), 11).

inc_fun() ->
    fun(X) -> maybe(X + 1) end.

