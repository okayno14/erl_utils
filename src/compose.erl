-module(compose).

-export([
    run_compose/2,
    run_conveyor/2,
    catch_wrap/1
]).

-export_type([result/0]).

-type result() :: {_Result, {error, _Reason}} | {error, _Reason} | _Result.

%%--------------------------------------------------------------------
%% @doc То же, что и run_conveyor/2, но слева-направо
-spec run_compose(AccFun :: fun(() -> result()), FunList :: [fun(() -> result())]) ->
    result().
%%--------------------------------------------------------------------
run_compose(AccFun, FunList) ->
    run_conveyor(AccFun, lists:reverse(FunList)).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% <pre>
%% Пропускает значение по конвейеру функций.
%% AccFun - функция, возвращающая начальное значение
%% FunList - список анонимных функций, по которым будет пропущен аккумулятор
%% pre:
%%   Функции из FunList не должны генерировать исключения
%% </pre>
%% @end
-spec run_conveyor(AccFun :: fun(() -> result()), FunList :: [fun(() -> result())]) ->
    result().
%%--------------------------------------------------------------------
run_conveyor(AccFun, FunList) ->
    run_conveyor_(undefined, [fun(_) -> AccFun() end | FunList]).

run_conveyor_(Acc, []) ->
    Acc;

run_conveyor_(Acc, [Fun | FunList]) ->
    case Fun(Acc) of
        ResultErr = {_Result, {error, _Reason}} ->
            ResultErr;

        ResultErr = {error, _Reason} ->
            ResultErr;

        Acc2 ->
            run_conveyor_(Acc2, FunList)
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

