-module(monad).

-export([pipe/3]).

%% RECOMMENDED
-export_type([
    monad/1,
    extract_ret/1,
    ffun/1,
    ffun/2
]).

-export_type([
    monad/0,
    extract_ret/0,
    ffun/0
]).

-type monad() :: monad().
-type monad(Y) :: monad(Y).

-type extract_ret() :: extract_ret(term()).
-type extract_ret(Y) ::
    %% Говорит раннеру пайпа о том,
    %% что необходимо поместить список функций в начало стека
    {dive, monad(_), [ffun()]} |
    Y.

-type ffun() :: ffun(term(), term()).
-type ffun(X) :: ffun(X, X).
-type ffun(X, Y) :: fun((X) -> monad(extract_ret(Y))).

-callback bind(Monad, ffun(X, Y)) -> Monad | monad(extract_ret(Y)) when
    Monad :: monad(extract_ret(X)).

%% Нужна для вытаскивывания зачёрнутого значения и оптимизации рекурсии пайпа
-callback extract(monad(X)) -> extract_ret(X).

%%--------------------------------------------------------------------
%% @doc
-spec pipe(Mod :: module(), Monad :: monad(), ListFun :: [ffun()]) ->
    monad().
%%--------------------------------------------------------------------
pipe(Mod, Monad, ListFun) ->
    pipe(
        fun Mod:bind/2,
        fun Mod:extract/1,
        Monad,
        [ListFun]
    ).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec pipe(
    %% Mod:bind/2
    BindFun :: fun((Monad, ffun(X, Y)) ->
        Monad | monad(extract_ret(Y))),
    %% Mod:extract/1
    ExtractFun :: fun((Monad) -> extract_ret(X)),
    Monad,
    ListFun :: [[ffun()]]
) ->
    monad()
when
    Monad :: monad(extract_ret(X)).
%%--------------------------------------------------------------------
pipe(_BindFun, _AccFun, Monad, []) ->
    Monad;

pipe(BindFun, ExtractFun, Monad, [[] | T]) ->
    pipe(BindFun, ExtractFun, Monad, T);

pipe(BindFun, ExtractFun, Monad, [H | T]) ->
    [H2 | T2] = H,
    Monad2 = BindFun(Monad, H2),
    case ExtractFun(Monad2) of
        {dive, Monad3, L2} ->
            L3 = [L2] ++ [T2 | T],
            pipe(BindFun, ExtractFun, Monad3, L3);

        _ok ->
            L2 = [T2 | T],
            pipe(BindFun, ExtractFun, Monad2, L2)
    end.
%%--------------------------------------------------------------------

