-module(monad).

-export([
    map/3,
    flatmap/3,
    extract/2
]).

%% RECOMMENDED
-export_type([
    monad/1,
    map_fun/2,
    flatmap_fun/2
]).

-type monad(X) :: either:either(X)
| maybe_:maybe_(X)
| validation:validation(X).

-type map_fun(X, Y) :: fun((X) -> Y).

-type flatmap_fun(X, Y) :: fun((X) -> monad(Y)).

-callback map(Monad :: monad(X), F :: map_fun(X, Y)) -> monad(Y).

-callback flatmap(Monad :: monad(X), F :: flatmap_fun(X, Y)) -> monad(Y).

%% Нужна для вытаскивывания зачёрнутого значения
-callback extract(Monad :: monad(X)) -> X.

-spec map(Mod :: module(), Monad :: monad(X), F :: map_fun(X, Y)) ->
    monad(Y).

map(Mod, Monad, F) ->
    Mod:map(Monad, F).

-spec flatmap(Mod :: module(), Monad :: monad(X), F :: flatmap_fun(X, Y)) ->
    monad(Y).

flatmap(Mod, Monad, F) ->
    Mod:flatmap(Monad, F).

-spec extract(Mod :: module(), Monad :: monad(X)) ->
    X.

extract(Mod, Monad) ->
    Mod:extract(Monad).

