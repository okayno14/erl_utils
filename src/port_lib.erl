-module(port_lib).

-export([
    run_cmd/1,
    run_cmd/2,
    run_interactive_cmd/1
]).

-define(CODE_SUCCESS, 0).

-export_type([
    output/0
]).

-type output() :: [nonempty_string()].

%% TODO сделать обёрточку для асинхронного запуска run_cmd

%%--------------------------------------------------------------------
%% @doc
%% Синхронный запуск CMD. Возвращаемый тип зависит от статуса выхода комманды.
%% Весь текстовый вывод вернётся в виде списка строк.
%% @end
-spec run_cmd(CMD :: string()) ->
    either:either(Data :: output()).
%%--------------------------------------------------------------------
run_cmd(CMD) ->
    case port_lib_stdlib:run_cmd(CMD) of
        {ok, Data} ->
            either:right(Data);
        {error, Data} ->
            either:left(Data)
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% TODO сделать реализацию с использованием grimsby
%% @deprecated
%% Задумывалась как способ вызова команд-фильтров.
%% Не работает, т.к. порт не позволяет отдельно закрыть stdin.
%% @end
-spec run_cmd(CMD :: string(), Input :: [string()] | string()) ->
    either:either(Data :: output()).
%%--------------------------------------------------------------------
run_cmd(_CMD, _Input) ->
    either:left("").
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Запуск интерактивной команды, которая перехватывает текущий stdin/stdout
-spec run_interactive_cmd(CMD :: nonempty_string()) ->
    either:either(integer()).
%%--------------------------------------------------------------------
run_interactive_cmd(CMD) ->
    case port_lib_stdlib:run_interactive_cmd(CMD) of
        {exit_status, ?CODE_SUCCESS} ->
            either:right(?CODE_SUCCESS);
        {exit_status, Status} ->
            either:left(Status)
    end.
%%--------------------------------------------------------------------

