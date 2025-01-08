-module(monad).

%% RECOMMENDED
-export_type([
    monad/1,

    map_fun/1,
    map_fun/2,

    flatmap_fun/1,
    flatmap_fun/2
]).

-export_type([
    monad/0,
    map_fun/0,
    flatmap_fun/0
]).

-type monad() :: monad().
-type monad(X) :: monad(X).

-type map_fun() :: map_fun(term(), term()).
-type map_fun(X) :: map_fun(X, X).
-type map_fun(X, Y) :: fun((X) -> Y).

-type flatmap_fun() :: flatmap_fun(term(), term()).
-type flatmap_fun(X) :: flatmap_fun(X, X).
-type flatmap_fun(X, Y) :: fun((X) -> monad(Y)).

-callback map(monad(X), map_fun(X, Y)) -> monad(Y).

-callback flatmap(monad(X), flatmap_fun(X, Y)) -> monad(Y).

%% Нужна для вытаскивывания зачёрнутого значения
-callback extract(monad(X)) -> X.

