-module(compose).

-include_lib("eunit/include/eunit.hrl").

-export([
    compose/1,
    pipe/1,

    run_compose/2,
    run_pipe/2,
    catch_wrap/1
]).

-export_type([
    result/0,
    result2/0,
    acc0/0
]).

%% Начальное значение аккумулятора для pipe/compose
-type acc0() :: fun(() -> result()) | term().

%% Конечный результат композиции
-type result() :: {_Result, {error, _Reason}} | {error, _Reason} | error | _Result.

%% Результат функций, собираемых в композицию
-type funlist2() :: [fun((_Acc) -> result2())].
-type result2() :: {dive, funlist2()} | {dive, _Acc, funlist2()} | result().

%%--------------------------------------------------------------------
%% @doc Возвращает анонимную функцию-композицию
-spec compose(FunList :: funlist2()) ->
    fun((AccFun :: fun(() -> result())) -> result()).
%%--------------------------------------------------------------------
compose(FunList) ->
    (curry:make_curry(fun compose:run_compose/2))(FunList).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Возвращает анонимную функцию-конвейер
-spec pipe(FunList :: funlist2()) ->
    fun((AccFun :: acc0()) -> result()).
%%--------------------------------------------------------------------
pipe(FunList) ->
    (curry:make_curry(fun compose:run_pipe/2))(FunList).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc То же, что и run_pipe/2, но слева-направо
-spec run_compose(FunList :: funlist2(), AccFun :: acc0()) ->
    result().
%%--------------------------------------------------------------------
run_compose(FunList, AccFun) ->
    run_pipe(lists:reverse(FunList), AccFun).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% <pre>
%% Пропускает значение по конвейеру функций.
%% FunList - список анонимных функций, по которым будет пропущен аккумулятор.
%%           Если одна из функций вернёт {error, _Reason}, то произойдёт остановка конвейера.
%%           Если одна из функций вернёт {dive, FunList}, то
%%           FunList будет положен в начало оставшегося конвейера (безопасно для стека вызовов).
%% AccFun - функция, возвращающая начальное значение; либо уже заранее определённый аккумулятор
%% pre:
%%   Функции из FunList не должны генерировать исключения
%% </pre>
%% @end
-spec run_pipe(FunList :: funlist2(), AccFun :: acc0()) ->
    result().
%%--------------------------------------------------------------------
run_pipe(FunList, AccFun) ->
    run_pipe_([[fun(_) -> AccFun() end | FunList]], undefined).

run_pipe_([], Acc) ->
    Acc;

run_pipe_([[] | T], Acc) ->
    run_pipe_(T, Acc);

run_pipe_([H | T], Acc) ->
     [H2 | T2] = H,
     case H2(Acc) of
        ResultErr = {_Result, {error, _Reason}} ->
            ResultErr;

        ResultErr = {error, _Reason} ->
            ResultErr;

        ResultErr = error ->
            ResultErr;

        {dive, L2} ->
            L3 = [L2] ++ [T2 | T],
            run_pipe_(L3, Acc);

        {dive, Acc2, L2} ->
            L3 = [L2] ++ [T2 | T],
            run_pipe_(L3, Acc2);

        Acc2 ->
            L2 = [T2 | T],
            run_pipe_(L2, Acc2)
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Оборачивает небезопасную функцию в result-паттерн
-spec catch_wrap(Fun :: fun()) ->
    {error, _Reason} | _Result.
%%--------------------------------------------------------------------
catch_wrap(Fun) ->
    case catch Fun() of
        {'EXIT', {Reason, _Stack}} ->
            {error, Reason};

        {'EXIT', Reason} ->
            {error, Reason};

        Result ->
            Result
    end.
%%--------------------------------------------------------------------

run_pipe_1_test() ->
    IncFun = fun(X) -> X + 1 end,
    11 =:=
    run_pipe([
        IncFun,
        fun(_X) -> {dive, [IncFun || _ <- lists:seq(1, 10)]} end
    ], fun() -> 0 end).

run_pipe_2_test() ->
    IncFun = fun(X) -> X + 1 end,
    {1, {error, my_reason}} =:=
    run_pipe([
        IncFun,
        fun(X) -> {X, {error, my_reason}} end,
        fun(_X) -> {dive, [IncFun || _ <- lists:seq(1, 10)]} end
    ], fun() -> 0 end).

run_pipe_3_test() ->
    IncFun = fun(X) -> X + 1 end,
    {error, my_reason} =:=
    run_pipe([
        IncFun,
        fun(_X) -> {error, my_reason} end,
        fun(_X) -> {dive, [IncFun || _ <- lists:seq(1, 10)]} end
    ], fun() -> 0 end).

run_pipe_4_test() ->
    IncFun = fun(X) -> X + 1 end,
    error =:=
    run_pipe([
        IncFun,
        fun(_X) -> error end,
        fun(_X) -> {dive, [IncFun || _ <- lists:seq(1, 10)]} end
    ], fun() -> 0 end).

