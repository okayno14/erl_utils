-define(stacktrace,
    erlang:element(2, erlang:process_info(erlang:self(), current_stacktrace))
).
