-module(port_lib).

-export([run_interactive_cmd/1]).

%%--------------------------------------------------------------------
%% @doc
%% <pre>
%% Запускает порт для CMD, чтение - текущий поток, запись - spawn_link.
%% НЕ РАБОТАЕТ С терминальными приложениями, т.к. они аттачатся к терминалу,
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

