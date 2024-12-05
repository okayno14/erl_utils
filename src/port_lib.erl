-module(port_lib).

-export([
    cmd_not/1,
    run_cmd/1,
    run_interactive_cmd/1
]).

-define(CODE_SUCCESS, 0).

%%--------------------------------------------------------------------
%% @doc Инверсия кода ошибки команды
-spec cmd_not(F :: fun(() -> {ok | error, _Data})) ->
    {ok | error, _Data}.
%%--------------------------------------------------------------------
cmd_not(F) ->
    case F() of
        {ok, Data} ->
            {error, Data};

        {error, Data} ->
            {ok, Data}
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec run_cmd(CMD :: nonempty_string()) ->
    {Status :: ok | error, Data :: nil() | [nonempty_string()]}.
%%--------------------------------------------------------------------
run_cmd(CMD) ->
    ReadDataFun =
    fun ReadData(P, Acc) ->
        receive
            {P, {data, Str}} ->
                ReadData(P, [Str | Acc])
        after
            0 ->
                lists:reverse(Acc)
        end
    end,

    P = erlang:open_port({spawn, CMD}, [exit_status]),
    receive
        {P, {exit_status, ?CODE_SUCCESS}} ->
            Status = ok;

        {P, {exit_status, _S}} ->
            Status = error
    end,
    Data = ReadDataFun(P, []),
    {Status, Data}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% <pre>
%% Запускает порт для CMD, чтение - текущий поток, запись - spawn_link.
%% НЕ РАБОТАЕТ С НЕКОТОРЫМИ терминальными приложениями, т.к. они аттачатся к терминалу,
%% а когда erl-машина аттачится к std-потокам, то приложения не могут адекватно выполнять обмен
%% </pre>
%% @end
-spec run_interactive_cmd(CMD :: nonempty_string()) ->
    ok.
%%--------------------------------------------------------------------
run_interactive_cmd(CMD) ->
    Port = erlang:open_port({spawn, CMD}, [exit_status, use_stdio]),
    _InputPid = spawn_link(fun()-> io:setopts([binary]), write_to_port(Port) end),
    read_from_port(Port).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec read_from_port(Port :: port()) ->
    ok.
%%--------------------------------------------------------------------
read_from_port(Port) ->
    receive
        {Port, {data, Data}} ->
            io:format("~s", [Data]),
            read_from_port(Port);

        {Port, {exit_status, 0}} ->
            erlang:port_close(Port),
            ok;

        {Port, {exit_status, StatusError}} ->
            io:format(standard_error, "Exited with code:~p~n", [StatusError]),
            erlang:port_close(Port),
            ok;

        _Msg ->
            io:format(standard_error, "Unknown msg:~p~n", [_Msg]),
            ok

    after 5000 ->
        read_from_port(Port)
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec write_to_port(Port :: port()) ->
    ok.
%%--------------------------------------------------------------------
write_to_port(Port) ->
    case io:get_chars("", 1) of
        {error, _} ->
            ok;

        eof ->
            ok;

        Input ->
            port_command(Port, Input),
            ok
    end,
    timer:sleep(16),
    write_to_port(Port).
%%--------------------------------------------------------------------

