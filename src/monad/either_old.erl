-module(either_old).

% -behaviour(monad).

-include_lib("eunit/include/eunit.hrl").

-export([
    left/1,
    right/1,
    is_right/1,
    is_left/1
]).

%% monad
-export([
    map/2,
    flatmap/2,
    extract/1
]).

-export_type([
    either/2,
    left/1,
    right/1
]).

%% Сюда пишется ошибка, обрывает цепочку исполнения
-record(left, {
    data :: term()
}).

-record(right, {
    data :: term()
}).

-opaque either(L, R) :: left(L) | right(R).

% -opaque either(X) :: left(X) | right(X).
-opaque left(X) :: #left{data :: X}.
-opaque right(X) :: #right{data :: X}.

%%--------------------------------------------------------------------
-spec map(
    Either :: either(L, A),
    fun((A) -> B)
) ->
    either(L, B).
%%--------------------------------------------------------------------
map(Left = #left{}, _F) ->
    Left;

map(Right = #right{}, F) ->
    right(F(extract(Right))).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec flatmap(
    Either :: either(L, A),
    fun((A) -> either(L, B))
) ->
    either(L, B).
%%--------------------------------------------------------------------
flatmap(Left = #left{}, _F) ->
    Left;

flatmap(Right = #right{}, F) ->
    F(extract(Right)).
%%--------------------------------------------------------------------

%%%===================================================================
%%% either-object
%%%===================================================================

%%--------------------------------------------------------------------
-spec left(X) ->
    left(X)
when
    X :: term().
%%--------------------------------------------------------------------
left(Data) ->
    #left{data = Data}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec right(X) ->
    right(X)
when
    X :: term().
%%--------------------------------------------------------------------
right(Data) ->
    #right{data = Data}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec is_right(Either :: either(_, _)) ->
    boolean().
%%--------------------------------------------------------------------
is_right(#right{}) ->
    true;
is_right(#left{}) ->
    false.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec is_left(Either :: either(_, _)) ->
    boolean().
%%--------------------------------------------------------------------
is_left(#left{}) ->
    true;
is_left(#right{}) ->
    false.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec extract(Either :: either(L, R)) ->
    L | R.
%%--------------------------------------------------------------------
extract(Either = #left{}) ->
    Either#left.data;

extract(Either = #right{}) ->
    Either#right.data.
%%--------------------------------------------------------------------

%%%===================================================================
%%% test
%%%===================================================================

flatmap_test_() ->
    [
        {"base test", fun case1/0},
        {"error test", fun case2/0},
        {"dive test", fun case3/0}
    ].

map_test_() ->
    [
        {"base test", fun case4/0},
        {"error test", fun case5/0}
    ].

case1() ->
    DB = #{1 => #{name => "a"}, 2 => #{name => "b"}},

    PersonFun = fun person/2,
    NameFun = fun name/1,

    Either = either_old:right(DB),

    %% Happy path
    Either2 =
        either_old:flatmap(
            either_old:flatmap(
                Either,
                (curry:curry_right(PersonFun))(2)
            ),
            NameFun
        ),
    ?assertEqual(either_old:extract(Either2), "b").

case2() ->
    DB = #{1 => #{name => "a"}, 2 => #{name => "b"}},

    PersonFun = fun person/2,
    NameFun = fun name/1,

    Either = either_old:right(DB),

    %% Fail
    Either2 = either_old:flatmap(either_old:flatmap(Either, (curry:curry_right(PersonFun))(3)), NameFun),
    ?assertEqual(either_old:extract(Either2), {error, not_found}).

case3() ->
    Either = either_old:right(1),
    IncFun = fun(X) -> either_old:right(X + 1) end,

    ?assertEqual(
        11,
        either_old:extract(
            compose:pipe(
                [(curry:curry_right(fun either_old:flatmap/2))(IncFun) || _ <- lists:seq(1, 10)],
                Either
            )
        )
    ).

case4() ->
    Either = either_old:right(1),
    IncFun = fun(X) -> X + 1 end,
    Either2 = either_old:map(either_old:map(either_old:map(Either, IncFun), IncFun), IncFun),
    ?assertEqual(4, either_old:extract(Either2)).

case5() ->
    Either = either_old:left(1),
    IncFun = fun(X) -> X + 1 end,
    Either2 = either_old:map(either_old:map(either_old:map(Either, IncFun), IncFun), IncFun),
    ?assertEqual(1, either_old:extract(Either2)).

person(DB, ID) ->
    case maps:get(ID, DB, undefined) of
        undefined ->
            either_old:left({error, not_found});

        Person ->
            either_old:right(Person)
    end.

name(Person) ->
    case maps:get(name, Person, undefined) of
        undefined ->
            either_old:left({error, invalid});

        Name ->
            either_old:right(Name)
    end.

