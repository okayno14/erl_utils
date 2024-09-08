-module(curry).

-export([
    make_curry/2,
    run_curry/2
]).

%%--------------------------------------------------------------------
%% @doc Прогоняет аргументы по цепочке карированных функций
-spec run_curry(FunCurried :: function(), Args :: list(term())) ->
    Result :: term().
%%--------------------------------------------------------------------
run_curry(FunCurried, Args) ->
    lists:foldl(fun(Elem, FunCurried1) -> FunCurried1(Elem) end, FunCurried, Args).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% <pre>
%% Позволяет выполнить каррирование функции, определённой в модуле.
%% Example:
%% F :: fun m:f/2 | fun f/2.
%% </pre>
%% @end
-spec make_curry(F :: function(), Arity :: pos_integer()) ->
    fun() | Result :: term().
%%--------------------------------------------------------------------
make_curry(F, Arity) when is_function(F) ->
    fun(X) -> make_curry_(F, Arity, X, []) end.

make_curry_(F, _Arity = 1, X, Args) ->
    Args2 = lists:reverse([X | Args]),
    erlang:apply(F, Args2);

make_curry_(F, Arity, X, Args) ->
    fun(X1) ->
        make_curry_(F, Arity - 1, X1, [X | Args])
    end.
%%--------------------------------------------------------------------

