-module(validation).

-behaviour(monad).

-include_lib("eunit/include/eunit.hrl").

-export([
    pipe/2,
    validation/1,
    validation_error/1,
    error_stack/1
]).

%% monad
-export([
    bind/2,
    extract/1
]).

-export_type([
    validation/0,
    validation/1,

    ffun/1,
    ffun/2,
    ffun/0
]).

-record(validation, {
    error_stack :: [monad:extract_ret()],
    data :: monad:extract_ret()
}).

-record(validation_error, {
    error_stack :: [monad:extract_ret()],
    data :: monad:extract_ret()
}).

-type validation() :: #validation{}.
-type validation(X) :: validation(X).

-type ffun() :: ffun(term(), term()).
-type ffun(X) :: ffun(X, X).
-type ffun(X, Y) :: monad:ffun(X, undefined) | monad:ffun(X, Y).

pipe(Validation, ListFun) ->
    monad:pipe(?MODULE, Validation, ListFun).

%%--------------------------------------------------------------------
%% @doc Скопировать ErrorStack, перетащить в новый объект,
%%
-spec bind(Validation, F :: ffun(X, Y)) ->
    Validation | validation(monad:extract_ret(Y))
when
    Validation :: validation(monad:extract_ret(X)).
%%--------------------------------------------------------------------
bind(Validation, F) ->
    ErrorStack = error_stack(Validation),
    Data = extract(Validation),

    case F(Data) of
        %% надо просто положить старый ErrorStack
        Validation2 = #validation{} ->
            push_error_stack(Validation2, ErrorStack);

        %% надо положить старую Data, расширить ErrorStack
        ValidationError = #validation_error{} ->
            push_error_stack(set_data(ValidationError, Data), ErrorStack)
    end.
%%--------------------------------------------------------------------

%%%===================================================================
%%% entity
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec validation(Data :: monad:extract_ret(X)) ->
    validation(X).
%%--------------------------------------------------------------------
validation(Data) ->
    #validation{data = Data, error_stack = []}.
%%--------------------------------------------------------------------

validation_error(ErrorStack) ->
    #validation_error{data = undefined, error_stack = ErrorStack}.

%%--------------------------------------------------------------------
%% @doc
-spec extract(Validation :: validation()) ->
    [monad:extract_ret()].
%%--------------------------------------------------------------------
extract(Validation = #validation{}) ->
    #validation{data = Data} = Validation,
    Data;

extract(ValidationError = #validation_error{}) ->
    #validation_error{data = Data} = ValidationError,
    Data.
%%--------------------------------------------------------------------

error_stack(Validation = #validation{}) ->
    #validation{error_stack = ErrorStack} = Validation,
    ErrorStack;

error_stack(ValidationError = #validation_error{}) ->
    #validation_error{error_stack = ErrorStack} = ValidationError,
    ErrorStack.

push_error_stack(Validation = #validation{}, ErrorStackTail) ->
    Validation#validation{
        error_stack = error_stack(Validation) ++ ErrorStackTail
    };

push_error_stack(ValidationError = #validation_error{}, ErrorStackTail) ->
    ValidationError#validation_error{
        error_stack = error_stack(ValidationError) ++ ErrorStackTail
    }.

set_data(Validation = #validation{}, Data) ->
    Validation#validation{data = Data};

set_data(ValidationError = #validation_error{}, Data) ->
    ValidationError#validation_error{data = Data}.

%%%===================================================================
%%% test
%%%===================================================================

base_test() ->
    UserInit = #{id => 100, name => "John Doe", age => 25},

    CheckIdFun = fun check_id/1,
    CheckNameFun = fun check_name/1,
    CheckAgeFun = fun check_age/1,

    Status = bind(bind(bind(validation(UserInit), CheckIdFun), CheckNameFun), CheckAgeFun),

    ?assertEqual(UserInit, extract(Status)),
    ?assertEqual([], error_stack(Status)).

error_test() ->
    UserInit = #{id => -1, name => "John Doe", age => 16},

    CheckIdFun = fun check_id/1,
    CheckNameFun = fun check_name/1,
    CheckAgeFun = fun check_age/1,

    Status = bind(bind(bind(validation(UserInit), CheckIdFun), CheckNameFun), CheckAgeFun),

    ?assertEqual(UserInit, extract(Status)),
    ?assertEqual([{error, {age, forbidden}}, {error, {id, negative_value}}], error_stack(Status)).

pipe_test() ->
    UserInit = #{id => 100, name => "John Doe", age => 25},

    CheckIdFun = fun check_id/1,
    CheckNameFun = fun check_name/1,
    CheckAgeFun = fun check_age/1,

    Status = pipe(validation(UserInit), [CheckIdFun, CheckNameFun, CheckAgeFun]),

    ?assertEqual(UserInit, extract(Status)),
    ?assertEqual([], error_stack(Status)).

check_id(User) ->
    case maps:get(id, User, undefined) of
        undefined ->
            Err = {error, {id, not_found}},
            validation_error([Err]);

        ID when is_integer(ID) andalso ID < 0 ->
            Err = {error, {id, negative_value}},
            validation_error([Err]);

        ID when is_integer(ID) ->
            validation(User);

        _ ->
            Err = {error, {id, unknown_error}},
            validation_error([Err])
    end.

check_name(User) ->
    case maps:get(name, User, undefined) of
        undefined ->
            Err = {error, {name, not_found}},
            validation_error([Err]);

        Name when is_list(Name) ->
            validation(User);

        _ ->
            Err = {error, {name, unknown_error}},
            validation_error([Err])
    end.

check_age(User) ->
    case maps:get(age, User, undefined) of
        undefined ->
            Err = {error, {age, not_found}},
            validation_error([Err]);

        ID when is_integer(ID) andalso ID < 18 ->
            Err = {error, {age, forbidden}},
            validation_error([Err]);

        ID when is_integer(ID) ->
            validation(User);

        _ ->
            Err = {error, {age, unknown_error}},
            validation_error([Err])
    end.

