-module(gleam@dynamic).
-compile(no_auto_import).

-export([from/1, unsafe_coerce/1, dynamic/1, bit_string/1, string/1, classify/1, int/1, float/1, bool/1, shallow_list/1, result/2, list/1, optional/1, field/2, element/2, tuple2/2, tuple3/3, tuple4/4, tuple5/5, tuple6/6, map/2, any/1, decode2/3, decode3/4, decode4/5, decode5/6, decode6/7, decode7/8, decode8/9, decode9/10]).
-export_type([dynamic/0, decode_error/0, unknown_tuple/0]).

-type dynamic() :: any().

-type decode_error() :: {decode_error, binary(), binary(), list(binary())}.

-type unknown_tuple() :: any().

-spec from(any()) -> dynamic().
from(A) ->
    gleam_stdlib:identity(A).

-spec unsafe_coerce(dynamic()) -> any().
unsafe_coerce(A) ->
    gleam_stdlib:identity(A).

-spec dynamic(dynamic()) -> {ok, dynamic()} | {error, list(decode_error())}.
dynamic(Value) ->
    {ok, Value}.

-spec bit_string(dynamic()) -> {ok, bitstring()} | {error, list(decode_error())}.
bit_string(Data) ->
    gleam_stdlib:decode_bit_string(Data).

-spec string(dynamic()) -> {ok, binary()} | {error, list(decode_error())}.
string(Data) ->
    decode_string(Data).

-spec map_errors(
    {ok, COC} | {error, list(decode_error())},
    fun((decode_error()) -> decode_error())
) -> {ok, COC} | {error, list(decode_error())}.
map_errors(Result, F) ->
    gleam@result:map_error(
        Result,
        fun(_capture) -> gleam@list:map(_capture, F) end
    ).

-spec decode_string(dynamic()) -> {ok, binary()} | {error, list(decode_error())}.
decode_string(Data) ->
    _pipe = bit_string(Data),
    _pipe@1 = map_errors(
        _pipe,
        fun(_capture) -> put_expected(_capture, <<"String"/utf8>>) end
    ),
    gleam@result:then(
        _pipe@1,
        fun(Raw) -> case gleam@bit_string:to_string(Raw) of
                {ok, String} ->
                    {ok, String};

                {error, nil} ->
                    {error,
                     [{decode_error,
                       <<"String"/utf8>>,
                       <<"BitString"/utf8>>,
                       []}]}
            end end
    ).

-spec classify(dynamic()) -> binary().
classify(Data) ->
    gleam_stdlib:classify_dynamic(Data).

-spec int(dynamic()) -> {ok, integer()} | {error, list(decode_error())}.
int(Data) ->
    gleam_stdlib:decode_int(Data).

-spec float(dynamic()) -> {ok, float()} | {error, list(decode_error())}.
float(Data) ->
    gleam_stdlib:decode_float(Data).

-spec bool(dynamic()) -> {ok, boolean()} | {error, list(decode_error())}.
bool(Data) ->
    gleam_stdlib:decode_bool(Data).

-spec shallow_list(dynamic()) -> {ok, list(dynamic())} |
    {error, list(decode_error())}.
shallow_list(Value) ->
    gleam_stdlib:decode_list(Value).

-spec result(
    fun((dynamic()) -> {ok, CPH} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CPJ} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, {ok, CPH} | {error, CPJ}} |
    {error, list(decode_error())}).
result(Decode_ok, Decode_error) ->
    fun(Value) -> case gleam_stdlib:decode_result(Value) of
            {error, _try} -> {error, _try};
            {ok, Inner_result} ->
                case Inner_result of
                    {ok, Raw} ->
                        case begin
                            _pipe = Decode_ok(Raw),
                            map_errors(
                                _pipe,
                                fun(_capture) ->
                                    push_path(_capture, <<"ok"/utf8>>)
                                end
                            )
                        end of
                            {error, _try@1} -> {error, _try@1};
                            {ok, Value@1} ->
                                {ok, {ok, Value@1}}
                        end;

                    {error, Raw@1} ->
                        case begin
                            _pipe@1 = Decode_error(Raw@1),
                            map_errors(
                                _pipe@1,
                                fun(_capture@1) ->
                                    push_path(_capture@1, <<"error"/utf8>>)
                                end
                            )
                        end of
                            {error, _try@2} -> {error, _try@2};
                            {ok, Value@2} ->
                                {ok, {error, Value@2}}
                        end
                end
        end end.

-spec list(fun((dynamic()) -> {ok, CPO} | {error, list(decode_error())})) -> fun((dynamic()) -> {ok,
                                                                                                 list(CPO)} |
    {error, list(decode_error())}).
list(Decoder_type) ->
    fun(Dynamic) -> case shallow_list(Dynamic) of
            {error, _try} -> {error, _try};
            {ok, List} ->
                _pipe = List,
                _pipe@1 = gleam@list:try_map(_pipe, Decoder_type),
                map_errors(
                    _pipe@1,
                    fun(_capture) -> push_path(_capture, <<"*"/utf8>>) end
                )
        end end.

-spec optional(fun((dynamic()) -> {ok, CPT} | {error, list(decode_error())})) -> fun((dynamic()) -> {ok,
                                                                                                     gleam@option:option(CPT)} |
    {error, list(decode_error())}).
optional(Decode) ->
    fun(Value) -> gleam_stdlib:decode_option(Value, Decode) end.

-spec field(
    any(),
    fun((dynamic()) -> {ok, CQD} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, CQD} | {error, list(decode_error())}).
field(Name, Inner_type) ->
    fun(Value) ->
        _pipe = Value,
        _pipe@1 = gleam_stdlib:decode_field(_pipe, Name),
        _pipe@2 = gleam@result:then(_pipe@1, Inner_type),
        map_errors(_pipe@2, fun(_capture) -> push_path(_capture, Name) end)
    end.

-spec element(
    integer(),
    fun((dynamic()) -> {ok, CQJ} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, CQJ} | {error, list(decode_error())}).
element(Index, Inner_type) ->
    fun(Data) -> case gleam_stdlib:decode_tuple(Data) of
            {error, _try} -> {error, _try};
            {ok, Tuple} ->
                Size = gleam_stdlib:size_of_tuple(Tuple),
                case case Index >= 0 of
                    true ->
                        case Index < Size of
                            true ->
                                gleam_stdlib:tuple_get(Tuple, Index);

                            false ->
                                at_least_decode_tuple_error(Index + 1, Data)
                        end;

                    false ->
                        case gleam@int:absolute_value(Index) =< Size of
                            true ->
                                gleam_stdlib:tuple_get(Tuple, Size + Index);

                            false ->
                                at_least_decode_tuple_error(
                                    gleam@int:absolute_value(Index),
                                    Data
                                )
                        end
                end of
                    {error, _try@1} -> {error, _try@1};
                    {ok, Data@1} ->
                        _pipe = Inner_type(Data@1),
                        map_errors(
                            _pipe,
                            fun(_capture) -> push_path(_capture, Index) end
                        )
                end
        end end.

-spec exact_decode_tuple_error(integer(), dynamic()) -> {ok, any()} |
    {error, list(decode_error())}.
exact_decode_tuple_error(Size, Data) ->
    S = case Size of
        0 ->
            <<""/utf8>>;

        _@1 ->
            <<"s"/utf8>>
    end,
    Error = begin
        _pipe = [<<"Tuple of "/utf8>>,
                 gleam@int:to_string(Size),
                 <<" element"/utf8>>,
                 S],
        _pipe@1 = gleam@string_builder:from_strings(_pipe),
        _pipe@2 = gleam@string_builder:to_string(_pipe@1),
        {decode_error, _pipe@2, classify(Data), []}
    end,
    {error, [Error]}.

-spec at_least_decode_tuple_error(integer(), dynamic()) -> {ok, any()} |
    {error, list(decode_error())}.
at_least_decode_tuple_error(Size, Data) ->
    S = case Size of
        0 ->
            <<""/utf8>>;

        _@1 ->
            <<"s"/utf8>>
    end,
    Error = begin
        _pipe = [<<"Tuple of at least "/utf8>>,
                 gleam@int:to_string(Size),
                 <<" element"/utf8>>,
                 S],
        _pipe@1 = gleam@string_builder:from_strings(_pipe),
        _pipe@2 = gleam@string_builder:to_string(_pipe@1),
        {decode_error, _pipe@2, classify(Data), []}
    end,
    {error, [Error]}.

-spec tuple_errors({ok, any()} | {error, list(decode_error())}, binary()) -> list(decode_error()).
tuple_errors(Result, Name) ->
    case Result of
        {ok, _@1} ->
            [];

        {error, Errors} ->
            gleam@list:map(
                Errors,
                fun(_capture) -> push_path(_capture, Name) end
            )
    end.

-spec assert_is_tuple(dynamic(), integer()) -> {ok, nil} |
    {error, list(decode_error())}.
assert_is_tuple(Value, Desired_size) ->
    Expected = gleam@string_builder:to_string(
        gleam@string_builder:from_strings(
            [<<"Tuple of "/utf8>>,
             gleam@int:to_string(Desired_size),
             <<" elements"/utf8>>]
        )
    ),
    case map_errors(
        gleam_stdlib:decode_tuple(Value),
        fun(_capture) -> put_expected(_capture, Expected) end
    ) of
        {error, _try} -> {error, _try};
        {ok, Tuple} ->
            case gleam_stdlib:size_of_tuple(Tuple) of
                Size when Size =:= Desired_size ->
                    {ok, nil};

                _@1 ->
                    exact_decode_tuple_error(Desired_size, Value)
            end
    end.

-spec put_expected(decode_error(), binary()) -> decode_error().
put_expected(Error, Expected) ->
    erlang:setelement(2, Error, Expected).

-spec push_path(decode_error(), any()) -> decode_error().
push_path(Error, Name) ->
    Name@1 = from(Name),
    Decoder = any(
        [fun string/1,
         fun(X) -> gleam@result:map(int(X), fun gleam@int:to_string/1) end]
    ),
    Name@3 = case Decoder(Name@1) of
        {ok, Name@2} ->
            Name@2;

        {error, _@1} ->
            _pipe = [<<"<"/utf8>>, classify(Name@1), <<">"/utf8>>],
            _pipe@1 = gleam@string_builder:from_strings(_pipe),
            gleam@string_builder:to_string(_pipe@1)
    end,
    erlang:setelement(4, Error, [Name@3 | erlang:element(4, Error)]).

-spec tuple2(
    fun((dynamic()) -> {ok, CRE} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CRG} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, {CRE, CRG}} | {error, list(decode_error())}).
tuple2(Decode1, Decode2) ->
    fun(Value) -> case assert_is_tuple(Value, 2) of
            {error, _try} -> {error, _try};
            {ok, _@1} ->
                {A, B} = unsafe_coerce(Value),
                case {Decode1(A), Decode2(B)} of
                    {{ok, A@1}, {ok, B@1}} ->
                        {ok, {A@1, B@1}};

                    {A@2, B@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = gleam@list:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        {error, _pipe@1}
                end
        end end.

-spec tuple3(
    fun((dynamic()) -> {ok, CRJ} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CRL} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CRN} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, {CRJ, CRL, CRN}} | {error, list(decode_error())}).
tuple3(Decode1, Decode2, Decode3) ->
    fun(Value) -> case assert_is_tuple(Value, 3) of
            {error, _try} -> {error, _try};
            {ok, _@1} ->
                {A, B, C} = unsafe_coerce(Value),
                case {Decode1(A), Decode2(B), Decode3(C)} of
                    {{ok, A@1}, {ok, B@1}, {ok, C@1}} ->
                        {ok, {A@1, B@1, C@1}};

                    {A@2, B@2, C@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = gleam@list:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        _pipe@2 = gleam@list:append(
                            _pipe@1,
                            tuple_errors(C@2, <<"2"/utf8>>)
                        ),
                        {error, _pipe@2}
                end
        end end.

-spec tuple4(
    fun((dynamic()) -> {ok, CRQ} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CRS} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CRU} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CRW} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, {CRQ, CRS, CRU, CRW}} |
    {error, list(decode_error())}).
tuple4(Decode1, Decode2, Decode3, Decode4) ->
    fun(Value) -> case assert_is_tuple(Value, 4) of
            {error, _try} -> {error, _try};
            {ok, _@1} ->
                {A, B, C, D} = unsafe_coerce(Value),
                case {Decode1(A), Decode2(B), Decode3(C), Decode4(D)} of
                    {{ok, A@1}, {ok, B@1}, {ok, C@1}, {ok, D@1}} ->
                        {ok, {A@1, B@1, C@1, D@1}};

                    {A@2, B@2, C@2, D@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = gleam@list:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        _pipe@2 = gleam@list:append(
                            _pipe@1,
                            tuple_errors(C@2, <<"2"/utf8>>)
                        ),
                        _pipe@3 = gleam@list:append(
                            _pipe@2,
                            tuple_errors(D@2, <<"3"/utf8>>)
                        ),
                        {error, _pipe@3}
                end
        end end.

-spec tuple5(
    fun((dynamic()) -> {ok, CRZ} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CSB} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CSD} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CSF} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CSH} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, {CRZ, CSB, CSD, CSF, CSH}} |
    {error, list(decode_error())}).
tuple5(Decode1, Decode2, Decode3, Decode4, Decode5) ->
    fun(Value) -> case assert_is_tuple(Value, 5) of
            {error, _try} -> {error, _try};
            {ok, _@1} ->
                {A, B, C, D, E} = unsafe_coerce(Value),
                case {Decode1(A),
                      Decode2(B),
                      Decode3(C),
                      Decode4(D),
                      Decode5(E)} of
                    {{ok, A@1}, {ok, B@1}, {ok, C@1}, {ok, D@1}, {ok, E@1}} ->
                        {ok, {A@1, B@1, C@1, D@1, E@1}};

                    {A@2, B@2, C@2, D@2, E@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = gleam@list:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        _pipe@2 = gleam@list:append(
                            _pipe@1,
                            tuple_errors(C@2, <<"2"/utf8>>)
                        ),
                        _pipe@3 = gleam@list:append(
                            _pipe@2,
                            tuple_errors(D@2, <<"3"/utf8>>)
                        ),
                        _pipe@4 = gleam@list:append(
                            _pipe@3,
                            tuple_errors(E@2, <<"4"/utf8>>)
                        ),
                        {error, _pipe@4}
                end
        end end.

-spec tuple6(
    fun((dynamic()) -> {ok, CSK} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CSM} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CSO} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CSQ} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CSS} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CSU} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, {CSK, CSM, CSO, CSQ, CSS, CSU}} |
    {error, list(decode_error())}).
tuple6(Decode1, Decode2, Decode3, Decode4, Decode5, Decode6) ->
    fun(Value) -> case assert_is_tuple(Value, 6) of
            {error, _try} -> {error, _try};
            {ok, _@1} ->
                {A, B, C, D, E, F} = unsafe_coerce(Value),
                case {Decode1(A),
                      Decode2(B),
                      Decode3(C),
                      Decode4(D),
                      Decode5(E),
                      Decode6(F)} of
                    {{ok, A@1},
                     {ok, B@1},
                     {ok, C@1},
                     {ok, D@1},
                     {ok, E@1},
                     {ok, F@1}} ->
                        {ok, {A@1, B@1, C@1, D@1, E@1, F@1}};

                    {A@2, B@2, C@2, D@2, E@2, F@2} ->
                        _pipe = tuple_errors(A@2, <<"0"/utf8>>),
                        _pipe@1 = gleam@list:append(
                            _pipe,
                            tuple_errors(B@2, <<"1"/utf8>>)
                        ),
                        _pipe@2 = gleam@list:append(
                            _pipe@1,
                            tuple_errors(C@2, <<"2"/utf8>>)
                        ),
                        _pipe@3 = gleam@list:append(
                            _pipe@2,
                            tuple_errors(D@2, <<"3"/utf8>>)
                        ),
                        _pipe@4 = gleam@list:append(
                            _pipe@3,
                            tuple_errors(E@2, <<"4"/utf8>>)
                        ),
                        _pipe@5 = gleam@list:append(
                            _pipe@4,
                            tuple_errors(F@2, <<"5"/utf8>>)
                        ),
                        {error, _pipe@5}
                end
        end end.

-spec map(
    fun((dynamic()) -> {ok, CSX} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CSZ} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, gleam@map:map_(CSX, CSZ)} |
    {error, list(decode_error())}).
map(Key_type, Value_type) ->
    fun(Value) -> case gleam_stdlib:decode_map(Value) of
            {error, _try} -> {error, _try};
            {ok, Map} ->
                case begin
                    _pipe = Map,
                    _pipe@1 = gleam@map:to_list(_pipe),
                    gleam@list:try_map(
                        _pipe@1,
                        fun(Pair) ->
                            {K, V} = Pair,
                            case begin
                                _pipe@2 = Key_type(K),
                                map_errors(
                                    _pipe@2,
                                    fun(_capture) ->
                                        push_path(_capture, <<"keys"/utf8>>)
                                    end
                                )
                            end of
                                {error, _try@1} -> {error, _try@1};
                                {ok, K@1} ->
                                    case begin
                                        _pipe@3 = Value_type(V),
                                        map_errors(
                                            _pipe@3,
                                            fun(_capture@1) ->
                                                push_path(
                                                    _capture@1,
                                                    <<"values"/utf8>>
                                                )
                                            end
                                        )
                                    end of
                                        {error, _try@2} -> {error, _try@2};
                                        {ok, V@1} ->
                                            {ok, {K@1, V@1}}
                                    end
                            end
                        end
                    )
                end of
                    {error, _try@3} -> {error, _try@3};
                    {ok, Pairs} ->
                        {ok, gleam@map:from_list(Pairs)}
                end
        end end.

-spec any(list(fun((dynamic()) -> {ok, CTI} | {error, list(decode_error())}))) -> fun((dynamic()) -> {ok,
                                                                                                      CTI} |
    {error, list(decode_error())}).
any(Decoders) ->
    fun(Data) -> case Decoders of
            [] ->
                {error,
                 [{decode_error, <<"another type"/utf8>>, classify(Data), []}]};

            [Decoder | Decoders@1] ->
                case Decoder(Data) of
                    {ok, Decoded} ->
                        {ok, Decoded};

                    {error, _@1} ->
                        (any(Decoders@1))(Data)
                end
        end end.

-spec decode2(
    fun((CTM, CTN) -> CTO),
    fun((dynamic()) -> {ok, CTM} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CTN} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, CTO} | {error, list(decode_error())}).
decode2(Constructor, T1, T2) ->
    fun(Value) -> case {T1(Value), T2(Value)} of
            {{ok, A}, {ok, B}} ->
                {ok, Constructor(A, B)};

            {A@1, B@1} ->
                {error, gleam@list:flatten([all_errors(A@1), all_errors(B@1)])}
        end end.

-spec decode3(
    fun((CTS, CTT, CTU) -> CTV),
    fun((dynamic()) -> {ok, CTS} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CTT} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CTU} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, CTV} | {error, list(decode_error())}).
decode3(Constructor, T1, T2, T3) ->
    fun(Value) -> case {T1(Value), T2(Value), T3(Value)} of
            {{ok, A}, {ok, B}, {ok, C}} ->
                {ok, Constructor(A, B, C)};

            {A@1, B@1, C@1} ->
                {error,
                 gleam@list:flatten(
                     [all_errors(A@1), all_errors(B@1), all_errors(C@1)]
                 )}
        end end.

-spec decode4(
    fun((CUA, CUB, CUC, CUD) -> CUE),
    fun((dynamic()) -> {ok, CUA} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CUB} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CUC} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CUD} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, CUE} | {error, list(decode_error())}).
decode4(Constructor, T1, T2, T3, T4) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X)} of
            {{ok, A}, {ok, B}, {ok, C}, {ok, D}} ->
                {ok, Constructor(A, B, C, D)};

            {A@1, B@1, C@1, D@1} ->
                {error,
                 gleam@list:flatten(
                     [all_errors(A@1),
                      all_errors(B@1),
                      all_errors(C@1),
                      all_errors(D@1)]
                 )}
        end end.

-spec decode5(
    fun((CUK, CUL, CUM, CUN, CUO) -> CUP),
    fun((dynamic()) -> {ok, CUK} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CUL} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CUM} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CUN} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CUO} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, CUP} | {error, list(decode_error())}).
decode5(Constructor, T1, T2, T3, T4, T5) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X), T5(X)} of
            {{ok, A}, {ok, B}, {ok, C}, {ok, D}, {ok, E}} ->
                {ok, Constructor(A, B, C, D, E)};

            {A@1, B@1, C@1, D@1, E@1} ->
                {error,
                 gleam@list:flatten(
                     [all_errors(A@1),
                      all_errors(B@1),
                      all_errors(C@1),
                      all_errors(D@1),
                      all_errors(E@1)]
                 )}
        end end.

-spec decode6(
    fun((CUW, CUX, CUY, CUZ, CVA, CVB) -> CVC),
    fun((dynamic()) -> {ok, CUW} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CUX} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CUY} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CUZ} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CVA} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CVB} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, CVC} | {error, list(decode_error())}).
decode6(Constructor, T1, T2, T3, T4, T5, T6) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X), T5(X), T6(X)} of
            {{ok, A}, {ok, B}, {ok, C}, {ok, D}, {ok, E}, {ok, F}} ->
                {ok, Constructor(A, B, C, D, E, F)};

            {A@1, B@1, C@1, D@1, E@1, F@1} ->
                {error,
                 gleam@list:flatten(
                     [all_errors(A@1),
                      all_errors(B@1),
                      all_errors(C@1),
                      all_errors(D@1),
                      all_errors(E@1),
                      all_errors(F@1)]
                 )}
        end end.

-spec decode7(
    fun((CVK, CVL, CVM, CVN, CVO, CVP, CVQ) -> CVR),
    fun((dynamic()) -> {ok, CVK} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CVL} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CVM} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CVN} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CVO} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CVP} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CVQ} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, CVR} | {error, list(decode_error())}).
decode7(Constructor, T1, T2, T3, T4, T5, T6, T7) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X), T5(X), T6(X), T7(X)} of
            {{ok, A}, {ok, B}, {ok, C}, {ok, D}, {ok, E}, {ok, F}, {ok, G}} ->
                {ok, Constructor(A, B, C, D, E, F, G)};

            {A@1, B@1, C@1, D@1, E@1, F@1, G@1} ->
                {error,
                 gleam@list:flatten(
                     [all_errors(A@1),
                      all_errors(B@1),
                      all_errors(C@1),
                      all_errors(D@1),
                      all_errors(E@1),
                      all_errors(F@1),
                      all_errors(G@1)]
                 )}
        end end.

-spec decode8(
    fun((CWA, CWB, CWC, CWD, CWE, CWF, CWG, CWH) -> CWI),
    fun((dynamic()) -> {ok, CWA} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CWB} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CWC} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CWD} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CWE} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CWF} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CWG} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CWH} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, CWI} | {error, list(decode_error())}).
decode8(Constructor, T1, T2, T3, T4, T5, T6, T7, T8) ->
    fun(X) -> case {T1(X), T2(X), T3(X), T4(X), T5(X), T6(X), T7(X), T8(X)} of
            {{ok, A},
             {ok, B},
             {ok, C},
             {ok, D},
             {ok, E},
             {ok, F},
             {ok, G},
             {ok, H}} ->
                {ok, Constructor(A, B, C, D, E, F, G, H)};

            {A@1, B@1, C@1, D@1, E@1, F@1, G@1, H@1} ->
                {error,
                 gleam@list:flatten(
                     [all_errors(A@1),
                      all_errors(B@1),
                      all_errors(C@1),
                      all_errors(D@1),
                      all_errors(E@1),
                      all_errors(F@1),
                      all_errors(G@1),
                      all_errors(H@1)]
                 )}
        end end.

-spec decode9(
    fun((CWS, CWT, CWU, CWV, CWW, CWX, CWY, CWZ, CXA) -> CXB),
    fun((dynamic()) -> {ok, CWS} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CWT} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CWU} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CWV} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CWW} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CWX} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CWY} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CWZ} | {error, list(decode_error())}),
    fun((dynamic()) -> {ok, CXA} | {error, list(decode_error())})
) -> fun((dynamic()) -> {ok, CXB} | {error, list(decode_error())}).
decode9(Constructor, T1, T2, T3, T4, T5, T6, T7, T8, T9) ->
    fun(X) ->
        case {T1(X), T2(X), T3(X), T4(X), T5(X), T6(X), T7(X), T8(X), T9(X)} of
            {{ok, A},
             {ok, B},
             {ok, C},
             {ok, D},
             {ok, E},
             {ok, F},
             {ok, G},
             {ok, H},
             {ok, I}} ->
                {ok, Constructor(A, B, C, D, E, F, G, H, I)};

            {A@1, B@1, C@1, D@1, E@1, F@1, G@1, H@1, I@1} ->
                {error,
                 gleam@list:flatten(
                     [all_errors(A@1),
                      all_errors(B@1),
                      all_errors(C@1),
                      all_errors(D@1),
                      all_errors(E@1),
                      all_errors(F@1),
                      all_errors(G@1),
                      all_errors(H@1),
                      all_errors(I@1)]
                 )}
        end
    end.

-spec all_errors({ok, any()} | {error, list(decode_error())}) -> list(decode_error()).
all_errors(Result) ->
    case Result of
        {ok, _@1} ->
            [];

        {error, Errors} ->
            Errors
    end.
