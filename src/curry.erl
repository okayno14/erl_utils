-module(curry).

-export([
    make_curry/1,
    run_curry/2
]).

%%--------------------------------------------------------------------
%% @doc
%% <pre>
%% Прогоняет аргументы по цепочке карированной функции
%% pre:
%%   length(Args) =&lt; arity(F), где F - искомая каррированная функция
%% </pre>
%% @end
-spec run_curry(FunCurried :: function(), Args :: list(term())) ->
    Result :: term().
%%--------------------------------------------------------------------
run_curry(FunCurried, Args) ->
    lists:foldl(fun(Elem, FunCurried1) -> FunCurried1(Elem) end, FunCurried, Args).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% <pre>
%% Позволяет выполнить каррирование функции, определённой в модуле:
%% F :: fun m:f/2 | fun f/2
%% Также работает и на прочих анонимных функциях.
%% </pre>
%% @end
-spec make_curry(F :: function()) ->
    fun() | Result :: term().
%%--------------------------------------------------------------------
make_curry(F) when is_function(F) ->
    Arity = proplists:get_value(arity, erlang:fun_info(F)),
    fun(X) -> make_curry_(F, Arity, X, []) end.

make_curry_(F, _Arity = 1, X, Args) ->
    Args2 = lists:reverse([X | Args]),
    erlang:apply(F, Args2);

make_curry_(F, Arity, X, Args) ->
    fun(X1) ->
        make_curry_(F, Arity - 1, X1, [X | Args])
    end.
%%--------------------------------------------------------------------

