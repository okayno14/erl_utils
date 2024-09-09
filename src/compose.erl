-module(compose).

-export([
    compose/1,
    pipe/1,

    run_compose/2,
    run_pipe/2,
    catch_wrap/1
]).

-export_type([result/0]).

-type result() :: {_Result, {error, _Reason}} | {error, _Reason} | _Result.

%%--------------------------------------------------------------------
%% @doc Возвращает анонимную функцию-композицию
-spec compose(FunList :: [fun(() -> result())]) ->
    fun((AccFun :: fun(() -> result())) -> result()).
%%--------------------------------------------------------------------
compose(FunList) ->
    (curry:make_curry(fun compose:run_compose/2))(FunList).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Возвращает анонимную функцию-конвейер
-spec pipe(FunList :: [fun(() -> result())]) ->
    fun((AccFun :: fun(() -> result())) -> result()).
%%--------------------------------------------------------------------
pipe(FunList) ->
    (curry:make_curry(fun compose:run_pipe/2))(FunList).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc То же, что и run_pipe/2, но слева-направо
-spec run_compose(FunList :: [fun(() -> result())], AccFun :: fun(() -> result())) ->
    result().
%%--------------------------------------------------------------------
run_compose(FunList, AccFun) ->
    run_pipe(lists:reverse(FunList), AccFun).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% <pre>
%% Пропускает значение по конвейеру функций.
%% FunList - список анонимных функций, по которым будет пропущен аккумулятор
%% AccFun - функция, возвращающая начальное значение
%% pre:
%%   Функции из FunList не должны генерировать исключения
%% </pre>
%% @end
-spec run_pipe(FunList :: [fun(() -> result())], AccFun :: fun(() -> result())) ->
    result().
%%--------------------------------------------------------------------
run_pipe(FunList, AccFun) ->
    run_pipe_(undefined, [fun(_) -> AccFun() end | FunList]).

run_pipe_(Acc, []) ->
    Acc;

run_pipe_(Acc, [Fun | FunList]) ->
    case Fun(Acc) of
        ResultErr = {_Result, {error, _Reason}} ->
            ResultErr;

        ResultErr = {error, _Reason} ->
            ResultErr;

        Acc2 ->
            run_pipe_(Acc2, FunList)
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

