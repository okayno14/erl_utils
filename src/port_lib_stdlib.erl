-module(port_lib_stdlib).

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

%%--------------------------------------------------------------------
%% @doc
%% Синхронный запуск CMD. Возвращаемый тип зависит от статуса выхода комманды.
%% Весь текстовый вывод вернётся в виде списка строк.
%% @end
-spec run_cmd(CMD :: string()) ->
    {ok | error, output()}.
%%--------------------------------------------------------------------
run_cmd(CMD) ->
    P = erlang:open_port({spawn, CMD}, [exit_status, stderr_to_stdout]),
    read_cmd_output_sync(P).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @deprecated
%% Задумывалась как способ вызова команд-фильтров.
%% Не работает, т.к. порт не позволяет отдельно закрыть stdin.
%% @end
-spec run_cmd(CMD :: string(), Input :: [string()] | string()) ->
    error.
%%--------------------------------------------------------------------
run_cmd(_CMD, _Input) ->
    error.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Запуск интерактивной команды, которая перехватывает текущий stdin/stdout
-spec run_interactive_cmd(CMD :: nonempty_string()) ->
    {exit_status, integer()}.
%%--------------------------------------------------------------------
run_interactive_cmd(CMD) ->
    Port = erlang:open_port({spawn, CMD}, [exit_status, nouse_stdio]),
    wait_exit_status(Port).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec read_cmd_output_sync(P :: port()) ->
    {ok | error, output()}.
%%--------------------------------------------------------------------
read_cmd_output_sync(P) ->
    case wait_exit_status(P) of
        {exit_status, ?CODE_SUCCESS} ->
            {ok, read_cmd_output_sync2(P, [])};
        {exit_status, _Status} ->
            {error, read_cmd_output_sync2(P, [])}
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Может вернуть последним элементом [], если одна последняя строка вывода закончилась разделителем
-spec read_cmd_output_sync2(P :: port(), Acc :: [string()]) ->
    Acc2 :: output().
%%--------------------------------------------------------------------
read_cmd_output_sync2(P, Acc) ->
    receive
        {P, {data, Str}} ->
            read_cmd_output_sync2(P, [Str | Acc])
    after 0 ->
        lists:flatmap(fun(Str) -> string:split(Str, "\n", all) end, lists:reverse(Acc))
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec wait_exit_status(P :: port()) ->
    {exit_status, integer()}.
%%--------------------------------------------------------------------
wait_exit_status(P) ->
    receive
        {P, Status = {exit_status, ?CODE_SUCCESS}} ->
            Status;
        {P, Status = {exit_status, Status}} ->
            Status
    end.
%%--------------------------------------------------------------------

