-module(io_lib).

-export([
	read_stdin_lines/0,
	read_lines/1
]).

%%--------------------------------------------------------------------
%% @doc Читает список строк из stdin
-spec read_stdin_lines() ->
    [] | string().
%%--------------------------------------------------------------------
read_stdin_lines() ->
	read_lines(standard_io).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Читает список строк из указанного io-потока
-spec read_lines(Device :: io:device()) ->
    [] | string().
%%--------------------------------------------------------------------
read_lines(Device) ->
	read_lines(Device, []).

read_lines(Device, L) ->
     case io:get_line(Device, "") of
		eof ->
			lists:reverse(L);

		String ->
			read_lines(Device, [string:trim(String, trailing) | L])
    end.
%%--------------------------------------------------------------------

