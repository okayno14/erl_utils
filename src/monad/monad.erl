-module(monad).

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

-callback flatmap(Monad, ffun(X, Y)) -> Monad | monad(extract_ret(Y)) when
    Monad :: monad(extract_ret(X)).

%% Нужна для вытаскивывания зачёрнутого значения и оптимизации рекурсии пайпа
-callback extract(monad(X)) -> extract_ret(X).

