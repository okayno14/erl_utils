-module(erl_utils).

-export([main/1]).

main([CMD]) ->
%     port_lib:run_interactive_cmd("cat -"),
%     port_lib:run_interactive_cmd("mc"),
    port_lib:run_interactive_cmd(CMD),
    ok;

main(_) ->
    port_lib:run_interactive_cmd("cat -"),
    ok.

