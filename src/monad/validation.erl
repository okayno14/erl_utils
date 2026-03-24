-module(validation).

-behaviour(monad).

-include_lib("eunit/include/eunit.hrl").

-export([
    validation/1,
    validation_error/1,
    extract_error_stack/1
]).

%% monad
-export([
    map/2,
    ok_flatmap/2,
    error_flatmap/2,
    flatmap/2,
    extract/1
]).

-export_type([
    validation_monad/1
]).

-record(validation, {
    error_stack :: [term()],
    data :: term()
}).

-record(validation_error, {
    error_stack :: [term()],
    data :: term()
}).

-opaque validation_monad(X) :: validation(X) | validation_error(X).
-type validation(X) :: #validation{data :: X}.
-type validation_error(X) :: #validation_error{data :: X}.

%%--------------------------------------------------------------------
-spec map(validation_monad(X), F :: monad:map_fun(X, Y)) ->
    validation_monad(Y)
when
    X :: term(),
    Y :: term().
%%--------------------------------------------------------------------
map(Validation, F) ->
    set_data(Validation, F(extract(Validation))).
%%--------------------------------------------------------------------

ok_flatmap(V = #validation{}, F) ->
    case extract_error_stack(V) of
        [] -> flatmap(V, F);
        _ -> V
    end.

error_flatmap(V = #validation{}, F) ->
    case extract_error_stack(V) of
        [] -> V;
        _ -> flatmap(V, F)
    end.

%%--------------------------------------------------------------------
-spec flatmap(validation_monad(X), F :: monad:flatmap_fun(X, Y)) ->
    validation_monad(Y)
when
    X :: term(),
    Y :: term().
%%--------------------------------------------------------------------
flatmap(Validation, F) ->
    ErrorStack = error_stack(Validation),
    Data = extract(Validation),

    %% TODO Таким образом под капотом можем добавлять к ошибке M,F,A чтобы можно было прочитать содержимое как при стактрейсе исключений
    %% TODO лучше вынести в макрос, чтобы было красивее
    %% TODO переделать: пусть принимает either2, но под капотом добавляет метаданные с ошибкой
    %% TODO добавить возможность явно построить ошибку
    {current_stacktrace, [_ | [{Mod, Fun, Arity, _Extra} | _]]} = erlang:process_info(
        erlang:self(), current_stacktrace
    ),

    %% TODO Пример, как должен будет выглядеть вывод
    % {error, [
    %     {eaccess, {M1, F1, A1}},
    %     {config_error, {M2, F2, A2}},
    % ]},
    %% Сразу видно, какой тип ошибки произошёл и в каком месте кода

    case F(Data) of
        %% надо просто положить старый ErrorStack
        Validation2 = #validation{} ->
            push_error_stack(Validation2, ErrorStack);
        %% надо положить старую Data, расширить ErrorStack
        ValidationError = #validation_error{} ->
            X = set_data(ValidationError, Data),
            push_error_stack(X, ErrorStack)
    end.
%%--------------------------------------------------------------------

%%%===================================================================
%%% entity
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec validation(Data) ->
    validation(Data)
when
    Data :: term().
%%--------------------------------------------------------------------
validation(Data) ->
    #validation{data = Data, error_stack = []}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec validation_error(ErrorStack :: list(X)) ->
    validation_error(X)
when
    X :: term().
%%--------------------------------------------------------------------
validation_error(ErrorStack) ->
    #validation_error{data = undefined, error_stack = ErrorStack}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec extract(Validation :: validation_monad(X)) ->
    X.
%%--------------------------------------------------------------------
extract(Validation = #validation{}) ->
    #validation{data = Data} = Validation,
    Data;

extract(ValidationError = #validation_error{}) ->
    #validation_error{data = Data} = ValidationError,
    Data.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec extract_error_stack(Validation :: validation_monad(_X)) ->
    list().
%%--------------------------------------------------------------------
extract_error_stack(Validation) ->
    lists:reverse(error_stack(Validation)).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec error_stack(validation_monad(_X)) ->
    list().
%%--------------------------------------------------------------------
error_stack(Validation = #validation{}) ->
    #validation{error_stack = ErrorStack} = Validation,
    ErrorStack;

error_stack(ValidationError = #validation_error{}) ->
    #validation_error{error_stack = ErrorStack} = ValidationError,
    ErrorStack.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Склеивает error_stack из нового объекта с накопленным ErrorStackTail
-spec push_error_stack(Validation :: validation_monad(X), ErrorStackTail :: list()) ->
    Validation2 :: validation_monad(X)
when
    X :: term().
%%--------------------------------------------------------------------
push_error_stack(Validation = #validation{}, ErrorStackTail) ->
    Validation#validation{
        error_stack = error_stack(Validation) ++ ErrorStackTail
    };

push_error_stack(ValidationError = #validation_error{}, ErrorStackTail) ->
    ValidationError#validation_error{
        error_stack = error_stack(ValidationError) ++ ErrorStackTail
    }.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec set_data(Validation :: validation_monad(_X), Data) ->
    Validation2 :: validation_monad(Data)
when
    Data :: term().
%%--------------------------------------------------------------------
set_data(Validation = #validation{}, Data) ->
    Validation#validation{data = Data};

set_data(ValidationError = #validation_error{}, Data) ->
    ValidationError#validation_error{data = Data}.
%%--------------------------------------------------------------------

%%%===================================================================
%%% test
%%%===================================================================

flatmap_test_() ->
    [
        {"base test", fun case1/0},
        {"error test", fun case2/0},
        {"pipe test", fun case3/0},
        {"pipe curry test", fun case4/0}
    ].

map_test_() ->
    [
        {"base test", fun case5/0}
    ].

case1() ->
    UserInit = #{id => 100, name => "John Doe", age => 25},

    CheckIdFun = monadize(fun check_id/1),
    CheckNameFun = monadize(fun check_name/1),
    CheckAgeFun = monadize(fun check_age/1),

    Status =
    validation:flatmap(
        validation:flatmap(
            validation:flatmap(
                validation:validation(UserInit),
                CheckIdFun
            ),
            CheckNameFun
        ),
        CheckAgeFun
    ),

    ?assertEqual(UserInit, validation:extract(Status)),
    ?assertEqual([], validation:extract_error_stack(Status)).

case2() ->
    UserInit = #{id => -1, name => "John Doe", age => 16},

    CheckIdFun = monadize(fun check_id/1),
    CheckNameFun = monadize(fun check_name/1),
    CheckAgeFun = monadize(fun check_age/1),

    Status = validation:flatmap(
        validation:flatmap(
            validation:flatmap(
                validation:validation(UserInit),
                CheckIdFun
            ),
            CheckNameFun
        ),
        CheckAgeFun
    ),

    ?assertEqual(UserInit, validation:extract(Status)),
    ?assertEqual([{error, {id, negative_value}}, {error, {age, forbidden}}], validation:extract_error_stack(Status)).

case3() ->
    UserInit = #{id => 100, name => "John Doe", age => 25},

    CheckIdFun = monadize(fun check_id/1),
    CheckNameFun = monadize(fun check_name/1),
    CheckAgeFun = monadize(fun check_age/1),

    Status =
        compose:pipe(
            [
                fun(Validation) -> validation:flatmap(Validation, CheckIdFun) end,
                fun(Validation) -> validation:flatmap(Validation, CheckNameFun) end,
                fun(Validation) -> validation:flatmap(Validation, CheckAgeFun) end
            ],
            validation:validation(UserInit)
        ),

    ?assertEqual(UserInit, validation:extract(Status)),
    ?assertEqual([], validation:extract_error_stack(Status)).

case4() ->
    UserInit = #{id => 100, name => "John Doe", age => 25},

    CheckIdFun = monadize(fun check_id/1),
    CheckNameFun = monadize(fun check_name/1),
    CheckAgeFun = monadize(fun check_age/1),

    Status =
        compose:pipe(
            [
                (curry:curry_right(fun validation:flatmap/2))(CheckIdFun),
                (curry:curry_right(fun validation:flatmap/2))(CheckNameFun),
                (curry:curry_right(fun validation:flatmap/2))(CheckAgeFun)
            ],
            validation:validation(UserInit)
        ),

    ?assertEqual(UserInit, validation:extract(Status)),
    ?assertEqual([], validation:extract_error_stack(Status)).

case5() ->
    UserInit = #{id => 100, name => "John Doe", age => 25},

    CheckIdFun = monadize(fun check_id/1),
    CheckNameFun = monadize(fun check_name/1),
    CheckAgeFun = monadize(fun check_age/1),

    Status =
    validation:map(
        validation:flatmap(
            validation:flatmap(
                validation:flatmap(
                    validation:validation(UserInit),
                    CheckIdFun
                ),
                CheckNameFun
            ),
            CheckAgeFun
        ),
        fun(UserArg) -> UserArg#{age => maps:get(age, UserArg) + 10} end
    ),

    ?assertEqual(35, maps:get(age, validation:extract(Status))),
    ?assertEqual([], validation:extract_error_stack(Status)).

%% @doc
%% <pre>
%% Оборачивает функции-валидаторы в апи either.
%% Позволяет разделить апи работы с сущностью от используемой монады.
%% </pre>
%% @end
monadize(F) ->
    fun(Data) ->
        case F(Data) of
            {ok, Value} ->
                validation:validation(Value);

            {error, ErrorStack} ->
                validation:validation_error(ErrorStack)
        end
    end.

check_id(User) ->
    case maps:get(id, User, undefined) of
        undefined ->
            Err = {error, {id, not_found}},
            {error, [Err]};

        ID when is_integer(ID) andalso ID < 0 ->
            Err = {error, {id, negative_value}},
            {error, [Err]};

        ID when is_integer(ID) ->
            {ok, User};

        _ ->
            Err = {error, {id, unknown_error}},
           {error, [Err]}
    end.

check_name(User) ->
    case maps:get(name, User, undefined) of
        undefined ->
            Err = {error, {name, not_found}},
            {error, [Err]};

        Name when is_list(Name) ->
            {ok, User};

        _ ->
            Err = {error, {name, unknown_error}},
            {error, [Err]}
    end.

check_age(User) ->
    case maps:get(age, User, undefined) of
        undefined ->
            Err = {error, {age, not_found}},
            {error, [Err]};

        ID when is_integer(ID) andalso ID < 18 ->
            Err = {error, {age, forbidden}},
            {error, [Err]};

        ID when is_integer(ID) ->
            {ok, User};

        _ ->
            Err = {error, {age, unknown_error}},
            {error, [Err]}
    end.

