-module(compose).

-include_lib("eunit/include/eunit.hrl").

-export([
    compose/2,
    pipe/2,
    if_else/4,
    match/3
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

%%--------------------------------------------------------------------
-spec when_(Predicate, FunTrue, X) -> Y
when
    Predicate :: fun((X) -> boolean()),
    FunTrue :: fun((X) -> Y).
%%--------------------------------------------------------------------
when_(Predicate, FunTrue, X) ->
    if_else(
        Predicate,
        FunTrue,
        fun(X1) -> X1 end,
        X
    ).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec if_else(Predicate, FunTrue, FunFalse, X) -> Y
when
    Predicate :: fun((X) -> boolean()),
    FunTrue :: fun((X) -> Y),
    FunFalse :: fun((X) -> Y).
%%--------------------------------------------------------------------
if_else(Predicate, FunTrue, FunFalse, X) ->
    match(
        {true, fun(_) -> FunTrue(X) end},
        {false, fun(_) -> FunFalse(X) end},
        Predicate(X)
    ).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec match(
    {A, FunA},
    {B, FunB},
    X
) -> Y
when
    FunA :: fun((X) -> Y),
    FunB :: fun((X) -> Y),
    A :: term(),
    B :: term().
%%--------------------------------------------------------------------
match({A, FunA}, {_B, _FunB}, A) -> FunA(A);
match({_A, _FunA}, {B, FunB}, B) -> FunB(B).
%%--------------------------------------------------------------------

%%%===================================================================
%%% TEST
%%%===================================================================

when_test_() ->
    [
        {"true branch when", fun true_branch_when/0},
        {"false branch when", fun false_branch_when/0}
    ].

if_else_test_() ->
    [
        {"true branch if_else", fun true_branch_if_else/0},
        {"false branch if_else", fun false_branch_if_else/0}
    ].

match_test_() ->
    [
        {"simple match", fun simple_match/0}
    ].

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

true_branch_when() ->
    IsEven = fun(X) -> X rem 2 == 0 end,
    Multiply = fun(X) -> X * 100 end,

    ?assertEqual(
        400,
        when_(
            IsEven,
            Multiply,
            4
        )
    ).

false_branch_when() ->
    IsEven = fun(X) -> X rem 2 == 0 end,
    Multiply = fun(X) -> X * 100 end,

    ?assertEqual(
        3,
        when_(
            IsEven,
            Multiply,
            3
        )
    ).

true_branch_if_else() ->
    IsHuman = fun
        ({mortal}) -> true;
        (_) -> false
    end,
    FunTrue = fun({mortal}) -> "true branch" end,
    FunFalse = fun({mortal}) -> "false branch" end,

    ?assertEqual(
        "true branch",
        if_else(
            IsHuman,
            FunTrue,
            FunFalse,
            {mortal}
        )
    ).

false_branch_if_else() ->
    IsHuman = fun
        ({mortal}) -> true;
        (_) -> false
    end,
    FunTrue = fun({immortal}) -> "true branch" end,
    FunFalse = fun({immortal}) -> "false branch" end,

    ?assertEqual(
        "false branch",
        if_else(
            IsHuman,
            FunTrue,
            FunFalse,
            {immortal}
        )
    ).

simple_match() ->
    OkFun = fun(_) -> ok end,
    ErrFun = fun(_) -> error end,

    ?assertEqual(
        ok,
        match(
            {a, OkFun},
            {b, ErrFun},
            a
        )
    ).

