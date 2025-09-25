-module(compose).

-include_lib("eunit/include/eunit.hrl").

-export([
    compose/2,
    pipe/2,
    ternary/3,
    match/4
]).

%% Результат функций, собираемых в композицию
-type funlist2() :: [fun((Acc :: term()) -> Acc2 :: term())].

%%--------------------------------------------------------------------
%% @doc То же, что и pipe/2, но слева-направо
-spec compose(Acc :: term(), FunList :: funlist2()) ->
    Acc2 :: term().
%%--------------------------------------------------------------------
compose(Acc, FunList) ->
    lists:foldr(
        fun(Fun, Acc2) when is_function(Fun, 1) -> Fun(Acc2) end, Acc, FunList
    ).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% <pre>
%% Пропускает значение по конвейеру функций.
%% FunList - список анонимных функций, по которым будет пропущен аккумулятор.
%% Acc - уже заранее определённый аккумулятор
%% pre:
%%   Функции из FunList не должны генерировать исключения
%% </pre>
%% @end
-spec pipe(Acc :: term(), FunList :: funlist2()) ->
    Acc2 :: term().
%%--------------------------------------------------------------------
pipe(Acc, FunList) ->
    lists:foldl(
        fun(Fun, Acc2) when is_function(Fun, 1) -> Fun(Acc2) end, Acc, FunList
    ).
%%--------------------------------------------------------------------

ternary(true, TrueFun, _FalseFun) ->
    TrueFun();
ternary(false, _TrueFun, FalseFun) ->
    FalseFun().

match(Val, Expect, TrueFun, FalseFun) ->
    case Val of
        Expect ->
            TrueFun();
        _ ->
            FalseFun()
    end.

run_pipe_1_test() ->
    IncFun = fun(X) -> X + 1 end,
    Result =
    pipe(0, [
        IncFun,
        IncFun,
        IncFun,
        IncFun,
        IncFun
    ]),
    ?assertEqual(5, Result).

