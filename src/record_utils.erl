%% @doc
%% Module for recursive data structure parsing with complex records
%% @end

-module(record_utils).

-export([to_list/2]).

%% @doc Transform data structure: all records converts to proplist with named keys
%% @param Record Record or data structure with records.
%% @param Module Module, which defined all records in Record.
%% @end
%%--------------------------------------------------------------------
-spec to_list(Record :: term(), Module :: module()) ->
    term()
    | {error, {code,  'cover_compiled' | 'preloaded' | non_existing}}
    | {error, {beam_lib, io_lib:chars()}}.
to_list(Record, Module) ->
    X = beam(Module),
    X2 = ast(X),
    X3 = record_definitions(X2),
    extract(
        map(
            X3,
            fun(RecordDefList) -> to_list_(Record, RecordDefList) end
        )
    ).

beam(Module) ->
    case code:which(Module) of
        Filename when is_list(Filename) -> right(Filename);
        Ret -> left({error, {code, Ret}})
    end.

ast(X) ->
    flatmap(
        X,
        fun(Filename) ->
            case beam_lib:chunks(Filename, [abstract_code]) of
                {
                    ok,
                    {_Mod, [{abstract_code, {_Version, Forms}}]}
                } ->
                    right(Forms);
                {error, Reason} ->
                    left({error, {beam_lib, beam_lib:format_error(Reason)}})
            end
        end
    ).

record_definitions(X2) ->
    map(
        X2,
        fun(Forms) ->
            [D || {attribute, _, record, {_RecordName1, _} = D} <- Forms]
        end
    ).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
to_list_(Record = #{}, RecordDefList) ->
    maps:map(
        fun(_K, V) -> to_list_(V, RecordDefList) end,
        Record
    );
to_list_(Record, RecordDefList) when is_list(Record) ->
    [to_list_(V, RecordDefList) || V <- Record];
to_list_({}, _RecordDefinition) ->
    {};
to_list_(Record, RecordDefList) when is_tuple(Record) ->
    RecordName = element(1, Record),
    X = record_definition(Record, RecordName, RecordDefList),
    X2 =
        cata(
            X,
            fun(Tuple) -> tuple_to_list(RecordDefList, Tuple) end,
            fun(_RecordDefinition = {_RecordName2, RecordFields}) ->
                record_to_list(RecordFields, RecordDefList, RecordName, Record)
            end
        ),
    extract(X2);
to_list_(V, _RecordDefList) ->
    V.

record_definition(Record, RecordName, RecordDefList) ->
    flatmap(
        right(Record),
        fun
            (Record2) when is_atom(RecordName) ->
                case
                    lists:search(
                        fun({RecordName2, _}) -> RecordName2 == RecordName end,
                        RecordDefList
                    )
                of
                    {value, RecordDefinition} -> right(RecordDefinition);
                    false -> left(Record2)
                end;
            (Record2) ->
                left(Record2)
        end
    ).

tuple_to_list(RecordDefList, Tuple) ->
    erlang:list_to_tuple(
        lists:map(
            fun(V) -> to_list_(V, RecordDefList) end,
            erlang:tuple_to_list(Tuple)
        )
    ).

record_to_list(RecordFields, RecordDefList, RecordName, Record) ->
    L =
        lists:zip(
            record_field_names(RecordFields),
            lists:seq(2, erlang:length(RecordFields) + 1)
        ),
    L2 =
        lists:map(
            fun({RecordFieldName, Pos}) ->
                {
                    RecordFieldName,
                    to_list_(erlang:element(Pos, Record), RecordDefList)
                }
            end,
            L
        ),
    [{record, RecordName} | L2].
%%--------------------------------------------------------------------

record_field_names(RecordFields) ->
    lists:map(fun record_field_name/1, RecordFields).

record_field_name({typed_record_field, FieldDef, _Type}) ->
    record_field_name(FieldDef);
record_field_name({record_field, _, {atom, _, Name}, _Default}) ->
    Name;
record_field_name({record_field, _, {atom, _, Name}}) ->
    Name.

left(X) -> {left, X}.

right(X) -> {right, X}.

flatmap({left, X}, _F) -> {left, X};
flatmap({right, X}, F) -> F(X).

map({left, X}, _F) -> {left, X};
map({right, X}, F) -> {right, F(X)}.

extract({left, X}) -> X;
extract({right, X}) -> X.

cata({left, X}, LF, _RF) -> {left, LF(X)};
cata({right, X}, _LF, RF) -> {right, RF(X)}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Tests
%%%===================================================================

-record(
    no_defaults_no_types,
    {
        a,
        b
    }
).

-record(
    defaults_no_types,
    {
        a = 1,
        b = "text"
    }
).

-record(
    no_defaults_types,
    {
        a :: integer(),
        b :: string()
    }
).

-record(
    defaults_types,
    {
        a = 1 :: integer(),
        b = "text" :: string()
    }
).

-record(
    node,
    {
        left :: #node{} | undefined,
        right :: #node{} | undefined,
        value = ok :: term()
    }
).

main_test_() ->
    [
        {"record no defaults, no types", fun no_defaults_no_types/0},
        {"record with defaults, no types", fun defaults_no_types/0},
        {"record no defaults, types", fun no_defaults_types/0},
        {"record defaults types", fun defaults_types/0},
        {"recursive record in record", fun recursive_record_in_record/0},
        {"record with a list of records", fun record_list_of_records/0},
        {"record with a proplist of records", fun record_proplist_of_records/0},
        {"record with a map of records", fun record_map_of_records/0},
        {"record with a tuple of records", fun record_tuple_of_records/0}
    ].

no_defaults_no_types() ->
    ?assertEqual(
        [
            {record, no_defaults_no_types},
            {a, 1},
            {b, "q"}
        ],
        record_utils:to_list(
            #no_defaults_no_types{a = 1, b = "q"},
            record_utils
        )
    ).

defaults_no_types() ->
    ?assertEqual(
        [
            {record, defaults_no_types},
            {a, 1},
            {b, "text"}
        ],
        record_utils:to_list(
            #defaults_no_types{},
            record_utils
        )
    ).

no_defaults_types() ->
    ?assertEqual(
        [
            {record, no_defaults_types},
            {a, 1},
            {b, "text"}
        ],
        record_utils:to_list(
            #no_defaults_types{a = 1, b = "text"},
            record_utils
        )
    ).

defaults_types() ->
    ?assertEqual(
        [
            {record, defaults_types},
            {a, 1},
            {b, "text"}
        ],
        record_utils:to_list(
            #defaults_types{a = 1, b = "text"},
            record_utils
        )
    ).

recursive_record_in_record() ->
    ?assertEqual(
        [
            {record, node},
            {left, [
                {record, node},
                {left, [
                    {record, node},
                    {left, [
                        {record, node},
                        {left, undefined},
                        {right, undefined},
                        {value, ok}
                    ]},
                    {right, undefined},
                    {value, ok}
                ]},
                {right, [
                    {record, node},
                    {left, undefined},
                    {right, undefined},
                    {value, ok}
                ]},
                {value, ok}
            ]},
            {right, undefined},
            {value, ok}
        ],
        record_utils:to_list(
            #node{
                left =
                    #node{
                        left =
                            #node{
                                left = #node{}
                            },
                        right = #node{}
                    }
            },
            record_utils
        )
    ).

record_list_of_records() ->
    ?assertEqual(
        [
            {record, node},
            {left, undefined},
            {right, undefined},
            {value, [
                [
                    {record, node},
                    {left, undefined},
                    {right, undefined},
                    {value, ok}
                ],
                [{record, defaults_types}, {a, 1}, {b, "text"}],
                [
                    {record, node},
                    {left, undefined},
                    {right, undefined},
                    {value, ok}
                ]
            ]}
        ],
        record_utils:to_list(
            #node{
                value = [#node{}, #defaults_types{}, #node{}]
            },
            record_utils
        )
    ).

record_proplist_of_records() ->
    ?assertEqual(
        [
            {record, node},
            {left, undefined},
            {right, undefined},
            {value, [
                {a, [
                    {record, node},
                    {left, undefined},
                    {right, undefined},
                    {value, ok}
                ]},
                {b, [{record, defaults_types}, {a, 1}, {b, "text"}]},
                {c, [
                    {record, node},
                    {left, undefined},
                    {right, undefined},
                    {value, ok}
                ]}
            ]}
        ],
        record_utils:to_list(
            #node{
                value = [{a, #node{}}, {b, #defaults_types{}}, {c, #node{}}]
            },
            record_utils
        )
    ).

record_map_of_records() ->
    ?assertEqual(
        [
            {record, node},
            {left, undefined},
            {right, undefined},
            {value, #{
                c =>
                    [
                        {record, node},
                        {left, undefined},
                        {right, undefined},
                        {value, ok}
                    ],
                b => [{record, defaults_types}, {a, 1}, {b, "text"}],
                a =>
                    [
                        {record, node},
                        {left, undefined},
                        {right, undefined},
                        {value, ok}
                    ]
            }}
        ],
        record_utils:to_list(
            #node{
                value = #{a => #node{}, b => #defaults_types{}, c => #node{}}
            },
            record_utils
        )
    ).

record_tuple_of_records() ->
    ?assertEqual(
        [
            {record, node},
            {left, undefined},
            {right, undefined},
            {value, {
                some_tag,
                [
                    {record, node},
                    {left, undefined},
                    {right, undefined},
                    {value, ok}
                ],
                [{record, defaults_types}, {a, 1}, {b, "text"}],
                [
                    {record, node},
                    {left, undefined},
                    {right, undefined},
                    {value, ok}
                ],
                {}
            }}
        ],
        record_utils:to_list(
            #node{
                value = {some_tag, #node{}, #defaults_types{}, #node{}, {}}
            },
            record_utils
        )
    ).

-endif.


