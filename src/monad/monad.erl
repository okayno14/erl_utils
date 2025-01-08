-module(monad).

%% RECOMMENDED
-export_type([
    monad/1,
    extract_ret/1,

    ffun2/1,
    ffun2/2,

    ffun/1,
    ffun/2
]).

-export_type([
    monad/0,
    extract_ret/0,
    ffun2/0,
    ffun/0
]).

-type monad() :: monad().
-type monad(Y) :: monad(Y).

-type extract_ret() :: extract_ret(term()).
-type extract_ret(Y) :: Y.

%% TODO переименовать в map_fun
-type ffun2() :: ffun2(term(), term()).
-type ffun2(X) :: ffun2(X, X).
-type ffun2(X, Y) :: fun((X) -> Y).

%% TODO переименовать в flatmap_fun
-type ffun() :: ffun(term(), term()).
-type ffun(X) :: ffun(X, X).
-type ffun(X, Y) :: fun((X) -> monad(extract_ret(Y))).

-callback map(Monad, ffun2(X, Y)) -> Monad | monad(extract_ret(Y))  when
    Monad :: monad(extract_ret(X)).

-callback flatmap(Monad, ffun(X, Y)) -> Monad | monad(extract_ret(Y)) when
    Monad :: monad(extract_ret(X)).

%% Нужна для вытаскивывания зачёрнутого значения
-callback extract(monad(X)) -> extract_ret(X).

