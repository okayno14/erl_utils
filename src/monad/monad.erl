-module(monad).

%% RECOMMENDED
-export_type([
    monad/1,
    extract_ret/1,

    map_fun/1,
    map_fun/2,

    flatmap_fun/1,
    flatmap_fun/2
]).

-export_type([
    monad/0,
    extract_ret/0,
    map_fun/0,
    flatmap_fun/0
]).

-type monad() :: monad().
-type monad(Y) :: monad(Y).

-type extract_ret() :: extract_ret(term()).
-type extract_ret(Y) :: Y.

-type map_fun() :: map_fun(term(), term()).
-type map_fun(X) :: map_fun(X, X).
-type map_fun(X, Y) :: fun((X) -> Y).

-type flatmap_fun() :: flatmap_fun(term(), term()).
-type flatmap_fun(X) :: flatmap_fun(X, X).
-type flatmap_fun(X, Y) :: fun((X) -> monad(extract_ret(Y))).

-callback map(Monad, map_fun(X, Y)) -> Monad | monad(extract_ret(Y))  when
    Monad :: monad(extract_ret(X)).

-callback flatmap(Monad, flatmap_fun(X, Y)) -> Monad | monad(extract_ret(Y)) when
    Monad :: monad(extract_ret(X)).

%% Нужна для вытаскивывания зачёрнутого значения
-callback extract(monad(X)) -> extract_ret(X).

