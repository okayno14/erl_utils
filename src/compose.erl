-module(compose).

-include_lib("eunit/include/eunit.hrl").

-export([
    compose/2,
    pipe/2,
    catch_wrap/1
]).

-export_type([
    result/0
]).


%% Результат функций, собираемых в композицию
-type funlist2() :: [fun((_Acc) -> result())].

%% Конечный результат композиции
-type result() :: Result :: term() | {{error, _Reason}, _Result} | {error, _Reason} | error.

%%--------------------------------------------------------------------
%% @doc То же, что и pipe/2, но слева-направо
-spec compose(Acc :: term(), FunList :: funlist2()) ->
    result().
%%--------------------------------------------------------------------
compose(Acc, FunList) ->
    pipe(Acc, lists:reverse(FunList)).
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
    result().
%%--------------------------------------------------------------------
pipe(Acc, []) ->
    Acc;

pipe(Acc, _FunList = [Fun | T]) ->
    case Fun(Acc) of
        ResultErr = {{error, _Reason}, _Acc2} ->
            ResultErr;

        ResultErr = {error, _Reason} ->
            ResultErr;

        ResultErr = error ->
            ResultErr;

        Acc2 ->
            pipe(Acc2, T)
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Оборачивает небезопасную функцию в error-tuple
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
    Result =
    pipe(0, [
        IncFun,
        IncFun,
        IncFun,
        IncFun,
        IncFun
    ]),
    ?assertEqual(5, Result).

run_pipe_2_test() ->
    IncFun = fun(X) -> X + 1 end,
    Result =
    pipe(0, [
        IncFun,
        IncFun,
        fun(_) -> error end,
        IncFun,
        IncFun
    ]),
    ?assertEqual(error, Result).

run_pipe_3_test() ->
    IncFun = fun(X) -> X + 1 end,
    Result =
    pipe(0, [
        IncFun,
        IncFun,
        fun(_) -> {error, my_reason} end,
        IncFun,
        IncFun
    ]),
    ?assertEqual({error, my_reason}, Result).

run_pipe_4_test() ->
    IncFun = fun(X) -> X + 1 end,
    Result =
    pipe(0, [
        IncFun,
        IncFun,
        fun(X) -> {{error, my_reason}, X} end,
        IncFun,
        IncFun
    ]),
    ?assertEqual({{error, my_reason}, 2}, Result).

