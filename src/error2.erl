%% @doc предоставляет конструкторы ошибок и набор типов для спецификации

-module(error2).

-export([error/1, code/1, additional_info/2, code_2/2, additional_info_2/3]).

-export_type([
    error/0,
    error_code/1,
    error_additional_info/2,
    error_code_2/2,
    error_additional_info_2/3,
    code/0, code/1,
    additional_info/0, additional_info/2,
    code_2/0, code_2/2,
    additional_info_2/0, additional_info_2/3
]).

-type error() ::
    error_code()
    | error_additional_info()
    | error_code_2()
    | error_additional_info_2().

-type error_code() :: {error, code()}.
-type error_code(Code) :: {error, code(Code)}.

-type error_additional_info() :: {error, additional_info()}.
-type error_additional_info(Code, AdditionalInfo) ::
    {error, additional_info(Code, AdditionalInfo)}.

-type error_code_2() :: {error, code_2()}.
-type error_code_2(Code, X) :: {error, code_2(Code, X)}.

-type error_additional_info_2() :: {error, additional_info_2()}.
-type error_additional_info_2(Code, AdditionalInfo, X) ::
    {error, additional_info_2(Code, AdditionalInfo, X)}.

-type code() ::
    Code :: atom().
-type additional_info() ::
    {Code :: atom(), AdditionalInfo :: term()}.

-type code_2() :: {
    Code :: atom(), {code()} | additional_info() | code_2() | additional_info_2()
}.
-type additional_info_2() :: {
    Code :: atom(),
    AdditionalInfo :: term(),
    {code()} | additional_info() | code_2() | additional_info_2()
}.

-type code(Code) ::
    Code.
-type additional_info(Code, AdditionalInfo) ::
    {Code, AdditionalInfo}.

-type code_2(Code, X) :: {
    Code, X
}.
-type additional_info_2(Code, AdditionalInfo, X) :: {
    Code,
    AdditionalInfo,
    X
}.

error(X) -> {error, X}.

-spec code(Code) -> code(Code) when Code :: atom().
code(Code) -> Code.

-spec additional_info(Code, AdditionalInfo) ->
    additional_info(Code, AdditionalInfo)
when
    Code :: atom, AdditionalInfo :: term().
additional_info(Code, AdditionalInfo) -> {Code, AdditionalInfo}.

%% TODO атом из 2-х, где первый элемент - всегда Code :: atom()
-spec code_2
    (code(), code()) -> code_2(code(), {code()});
    (code(), additional_info()) -> code_2(code(), additional_info());
    (code(), code_2()) -> code_2(code(), code_2());
    (code(), additional_info_2()) -> code_2(code(), additional_info_2()).
code_2(Code, Code2) when is_atom(Code2) -> {Code, {Code2}};
code_2(Code, X = {Code2, _}) when is_atom(Code2) -> {Code, X}.

-spec additional_info_2
    (code(), AdditionalInfo :: term(), code()) ->
        additional_info_2(code(), AdditionalInfo :: term(), {code()});
    (code(), AdditionalInfo :: term(), additional_info()) ->
        additional_info_2(code(), AdditionalInfo :: term(), additional_info());
    (code(), AdditionalInfo :: term(), code_2()) ->
        additional_info_2(code(), AdditionalInfo :: term(), code_2());
    (code(), AdditionalInfo :: term(), additional_info_2()) ->
        additional_info_2(code(), AdditionalInfo :: term(), additional_info_2()).
additional_info_2(Code, AdditionalInfo, Code2) when is_atom(Code2) ->
    {Code, AdditionalInfo, {Code2}};
additional_info_2(Code, AdditionalInfo, X = {Code2, _}) when is_atom(Code2) ->
    {Code, AdditionalInfo, X}.
