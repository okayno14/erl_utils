-module(maybe).

-include_lib("eunit/include/eunit.hrl").

-export([
    pipe/2,
    bind/2,

    maybe/1,
    acc/1
]).

-export_type([
    maybe/0,
    maybe/1,

    ffun/0,
    ffun/1,
    ffun/2
]).

-record(maybe, {acc :: {dive, Maybe :: maybe(), L :: [ffun()]} | term()}).

-type maybe() :: maybe(term()).
-type maybe(_V) :: #maybe{}.

-type ffun() :: ffun(term(), term()).
-type ffun(X) :: ffun(X, X).
-type ffun(X, Y) :: fun((X) -> Y).

%%--------------------------------------------------------------------
%% @doc
-spec pipe(
    Maybe :: maybe(),
    ListFun :: [ffun()]
) ->
    maybe().
%%--------------------------------------------------------------------
pipe(Maybe, ListFun) ->
    BindFun = fun bind/2,
    pipe_(BindFun, Maybe, [ListFun]).

pipe_(_BindFun, Maybe, []) ->
    Maybe;

pipe_(BindFun, Maybe, [[] | T]) ->
    pipe_(BindFun, Maybe, T);

pipe_(BindFun, Maybe, [H | T]) ->
    [H2 | T2] = H,
    Maybe2 = BindFun(Maybe, H2),
    case acc(Maybe2) of
        {dive, Maybe3, L2} ->
            L3 = [L2] ++ [T2 | T],
            pipe_(BindFun, Maybe3, L3);

        _ ->
            L2 = [T2 | T],
            pipe_(BindFun, Maybe2, L2)
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec bind(Maybe :: maybe(), F :: ffun()) ->
    Maybe2 :: maybe().
%%--------------------------------------------------------------------
bind(Maybe = #maybe{}, F) ->
    case acc(Maybe) of
        {error, _} ->
            Maybe;

        _ ->
            maybe(F(acc(Maybe)))
    end.
%%--------------------------------------------------------------------

%%%===================================================================
%%% maybe-object
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec maybe(Status) ->
    maybe(Status).
%%--------------------------------------------------------------------
maybe(Status) ->
    #maybe{acc = Status}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec acc(Maybe :: maybe()) ->
    _Status.
%%--------------------------------------------------------------------
acc(Maybe = #maybe{}) ->
    Maybe#maybe.acc.
%%--------------------------------------------------------------------

%%%===================================================================
%%% test
%%%===================================================================

base_test() ->
    Maybe = maybe(1),
    IncFun = fun(X) -> X + 1 end,
    Maybe2 = bind(bind(bind(Maybe, IncFun), IncFun), IncFun),
    ?assertEqual(acc(Maybe2), 4).

pipe_test() ->
    Maybe = maybe(1),
    IncFun = fun(X) -> X + 1 end,
    Maybe2 = pipe(Maybe, [IncFun, IncFun, IncFun]),
    ?assertEqual(acc(Maybe2), 4).

error_test() ->
    Maybe = maybe(1),
    IncFun = fun(X) -> X + 1 end,
    Error = {error, my_reason},
    Maybe2 = pipe(Maybe, [IncFun, IncFun, IncFun, fun(_) -> Error end, IncFun, IncFun]),
    ?assertEqual(acc(Maybe2), Error).

