-module(gleam@iterator).
-compile(no_auto_import).

-export([unfold/2, repeatedly/1, repeat/1, from_list/1, fold/3, run/1, to_list/1, step/1, take/2, drop/2, map/2, append/2, flatten/1, flat_map/2, filter/2, cycle/1, range/2, find/2, index/1, iterate/2, take_while/2, drop_while/2, scan/3, zip/2, chunk/2, sized_chunk/2, intersperse/2, any/2, all/2, group/2, reduce/2, last/1, empty/0, once/1, single/1, interleave/2, fold_until/3, try_fold/3, first/1, at/2]).
-export_type([action/1, iterator/1, step/2, chunk/2, sized_chunk/1]).

-type action(BOQ) :: stop | {continue, BOQ, fun(() -> action(BOQ))}.

-opaque iterator(BOR) :: {iterator, fun(() -> action(BOR))}.

-type step(BOS, BOT) :: {next, BOS, BOT} | done.

-type chunk(BOU, BOV) :: {another_by,
                          list(BOU),
                          BOV,
                          BOU,
                          fun(() -> action(BOU))} |
    {last_by, list(BOU)}.

-type sized_chunk(BOW) :: {another, list(BOW), fun(() -> action(BOW))} |
    {last, list(BOW)} |
    no_more.

-spec stop() -> action(any()).
stop() ->
    stop.

-spec do_unfold(BPB, fun((BPB) -> step(BPC, BPB))) -> fun(() -> action(BPC)).
do_unfold(Initial, F) ->
    fun() -> case F(Initial) of
            {next, X, Acc} ->
                {continue, X, do_unfold(Acc, F)};

            done ->
                stop
        end end.

-spec unfold(BPG, fun((BPG) -> step(BPH, BPG))) -> iterator(BPH).
unfold(Initial, F) ->
    _pipe = Initial,
    _pipe@1 = do_unfold(_pipe, F),
    {iterator, _pipe@1}.

-spec repeatedly(fun(() -> BPL)) -> iterator(BPL).
repeatedly(F) ->
    unfold(nil, fun(_) -> {next, F(), nil} end).

-spec repeat(BPN) -> iterator(BPN).
repeat(X) ->
    repeatedly(fun() -> X end).

-spec from_list(list(BPP)) -> iterator(BPP).
from_list(List) ->
    Yield = fun(Acc) -> case Acc of
            [] ->
                done;

            [Head | Tail] ->
                {next, Head, Tail}
        end end,
    unfold(List, Yield).

-spec do_fold(fun(() -> action(BPS)), fun((BPU, BPS) -> BPU), BPU) -> BPU.
do_fold(Continuation, F, Accumulator) ->
    case Continuation() of
        {continue, Elem, Next} ->
            do_fold(Next, F, F(Accumulator, Elem));

        stop ->
            Accumulator
    end.

-spec fold(iterator(BPV), BPX, fun((BPX, BPV) -> BPX)) -> BPX.
fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_fold(_pipe, F, Initial).

-spec run(iterator(any())) -> nil.
run(Iterator) ->
    fold(Iterator, nil, fun(_, _) -> nil end).

-spec to_list(iterator(BQA)) -> list(BQA).
to_list(Iterator) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, [], fun(Acc, E) -> [E | Acc] end),
    gleam@list:reverse(_pipe@1).

-spec step(iterator(BQD)) -> step(BQD, iterator(BQD)).
step(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            done;

        {continue, E, A} ->
            {next, E, {iterator, A}}
    end.

-spec do_take(fun(() -> action(BQI)), integer()) -> fun(() -> action(BQI)).
do_take(Continuation, Desired) ->
    fun() -> case Desired > 0 of
            false ->
                stop;

            true ->
                case Continuation() of
                    stop ->
                        stop;

                    {continue, E, Next} ->
                        {continue, E, do_take(Next, Desired - 1)}
                end
        end end.

-spec take(iterator(BQL), integer()) -> iterator(BQL).
take(Iterator, Desired) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_take(_pipe, Desired),
    {iterator, _pipe@1}.

-spec do_drop(fun(() -> action(BQO)), integer()) -> action(BQO).
do_drop(Continuation, Desired) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case Desired > 0 of
                true ->
                    do_drop(Next, Desired - 1);

                false ->
                    {continue, E, Next}
            end
    end.

-spec drop(iterator(BQR), integer()) -> iterator(BQR).
drop(Iterator, Desired) ->
    _pipe = fun() -> do_drop(erlang:element(2, Iterator), Desired) end,
    {iterator, _pipe}.

-spec do_map(fun(() -> action(BQU)), fun((BQU) -> BQW)) -> fun(() -> action(BQW)).
do_map(Continuation, F) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, F(E), do_map(Continuation@1, F)}
        end end.

-spec map(iterator(BQY), fun((BQY) -> BRA)) -> iterator(BRA).
map(Iterator, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_map(_pipe, F),
    {iterator, _pipe@1}.

-spec do_append(fun(() -> action(BRC)), fun(() -> action(BRC))) -> action(BRC).
do_append(First, Second) ->
    case First() of
        {continue, E, First@1} ->
            {continue, E, fun() -> do_append(First@1, Second) end};

        stop ->
            Second()
    end.

-spec append(iterator(BRG), iterator(BRG)) -> iterator(BRG).
append(First, Second) ->
    _pipe = fun() ->
        do_append(erlang:element(2, First), erlang:element(2, Second))
    end,
    {iterator, _pipe}.

-spec do_flatten(fun(() -> action(iterator(BRK)))) -> action(BRK).
do_flatten(Flattened) ->
    case Flattened() of
        stop ->
            stop;

        {continue, It, Next_iterator} ->
            do_append(
                erlang:element(2, It),
                fun() -> do_flatten(Next_iterator) end
            )
    end.

-spec flatten(iterator(iterator(BRO))) -> iterator(BRO).
flatten(Iterator) ->
    _pipe = fun() -> do_flatten(erlang:element(2, Iterator)) end,
    {iterator, _pipe}.

-spec flat_map(iterator(BRS), fun((BRS) -> iterator(BRU))) -> iterator(BRU).
flat_map(Iterator, F) ->
    _pipe = Iterator,
    _pipe@1 = map(_pipe, F),
    flatten(_pipe@1).

-spec do_filter(fun(() -> action(BRX)), fun((BRX) -> boolean())) -> action(BRX).
do_filter(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Iterator} ->
            case Predicate(E) of
                true ->
                    {continue, E, fun() -> do_filter(Iterator, Predicate) end};

                false ->
                    do_filter(Iterator, Predicate)
            end
    end.

-spec filter(iterator(BSA), fun((BSA) -> boolean())) -> iterator(BSA).
filter(Iterator, Predicate) ->
    _pipe = fun() -> do_filter(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-spec cycle(iterator(BSD)) -> iterator(BSD).
cycle(Iterator) ->
    _pipe = repeat(Iterator),
    flatten(_pipe).

-spec range(integer(), integer()) -> iterator(integer()).
range(Start, Stop) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            once(fun() -> Start end);

        gt ->
            unfold(Start, fun(Current) -> case Current < Stop of
                        false ->
                            {next, Current, Current - 1};

                        true ->
                            done
                    end end);

        lt ->
            unfold(Start, fun(Current@1) -> case Current@1 > Stop of
                        false ->
                            {next, Current@1, Current@1 + 1};

                        true ->
                            done
                    end end)
    end.

-spec do_find(fun(() -> action(BSH)), fun((BSH) -> boolean())) -> {ok, BSH} |
    {error, nil}.
do_find(Continuation, F) ->
    case Continuation() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            case F(E) of
                true ->
                    {ok, E};

                false ->
                    do_find(Next, F)
            end
    end.

-spec find(iterator(BSL), fun((BSL) -> boolean())) -> {ok, BSL} | {error, nil}.
find(Haystack, Is_desired) ->
    _pipe = erlang:element(2, Haystack),
    do_find(_pipe, Is_desired).

-spec do_index(fun(() -> action(BSP)), integer()) -> fun(() -> action({integer(),
                                                                       BSP})).
do_index(Continuation, Next) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Continuation@1} ->
                {continue, {Next, E}, do_index(Continuation@1, Next + 1)}
        end end.

-spec index(iterator(BSS)) -> iterator({integer(), BSS}).
index(Iterator) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_index(_pipe, 0),
    {iterator, _pipe@1}.

-spec iterate(BSV, fun((BSV) -> BSV)) -> iterator(BSV).
iterate(Initial, F) ->
    unfold(Initial, fun(Element) -> {next, Element, F(Element)} end).

-spec do_take_while(fun(() -> action(BSX)), fun((BSX) -> boolean())) -> fun(() -> action(BSX)).
do_take_while(Continuation, Predicate) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, E, Next} ->
                case Predicate(E) of
                    false ->
                        stop;

                    true ->
                        {continue, E, do_take_while(Next, Predicate)}
                end
        end end.

-spec take_while(iterator(BTA), fun((BTA) -> boolean())) -> iterator(BTA).
take_while(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_take_while(_pipe, Predicate),
    {iterator, _pipe@1}.

-spec do_drop_while(fun(() -> action(BTD)), fun((BTD) -> boolean())) -> action(BTD).
do_drop_while(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            case Predicate(E) of
                false ->
                    {continue, E, Next};

                true ->
                    do_drop_while(Next, Predicate)
            end
    end.

-spec drop_while(iterator(BTG), fun((BTG) -> boolean())) -> iterator(BTG).
drop_while(Iterator, Predicate) ->
    _pipe = fun() -> do_drop_while(erlang:element(2, Iterator), Predicate) end,
    {iterator, _pipe}.

-spec do_scan(fun(() -> action(BTJ)), fun((BTL, BTJ) -> BTL), BTL) -> fun(() -> action(BTL)).
do_scan(Continuation, F, Accumulator) ->
    fun() -> case Continuation() of
            stop ->
                stop;

            {continue, El, Next} ->
                Accumulated = F(Accumulator, El),
                {continue, Accumulated, do_scan(Next, F, Accumulated)}
        end end.

-spec scan(iterator(BTN), BTP, fun((BTP, BTN) -> BTP)) -> iterator(BTP).
scan(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_scan(_pipe, F, Initial),
    {iterator, _pipe@1}.

-spec do_zip(fun(() -> action(BTR)), fun(() -> action(BTT))) -> fun(() -> action({BTR,
                                                                                  BTT})).
do_zip(Left, Right) ->
    fun() -> case Left() of
            stop ->
                stop;

            {continue, El_left, Next_left} ->
                case Right() of
                    stop ->
                        stop;

                    {continue, El_right, Next_right} ->
                        {continue,
                         {El_left, El_right},
                         do_zip(Next_left, Next_right)}
                end
        end end.

-spec zip(iterator(BTW), iterator(BTY)) -> iterator({BTW, BTY}).
zip(Left, Right) ->
    _pipe = do_zip(erlang:element(2, Left), erlang:element(2, Right)),
    {iterator, _pipe}.

-spec next_chunk(fun(() -> action(BUE)), fun((BUE) -> BUG), BUG, list(BUE)) -> chunk(BUE, BUG).
next_chunk(Continuation, F, Previous_key, Current_chunk) ->
    case Continuation() of
        stop ->
            {last_by, gleam@list:reverse(Current_chunk)};

        {continue, E, Next} ->
            Key = F(E),
            case Key =:= Previous_key of
                true ->
                    next_chunk(Next, F, Key, [E | Current_chunk]);

                false ->
                    {another_by,
                     gleam@list:reverse(Current_chunk),
                     Key,
                     E,
                     Next}
            end
    end.

-spec do_chunk(fun(() -> action(BUK)), fun((BUK) -> BUM), BUM, BUK) -> action(list(BUK)).
do_chunk(Continuation, F, Previous_key, Previous_element) ->
    case next_chunk(Continuation, F, Previous_key, [Previous_element]) of
        {last_by, Chunk} ->
            {continue, Chunk, fun stop/0};

        {another_by, Chunk@1, Key, El, Next} ->
            {continue, Chunk@1, fun() -> do_chunk(Next, F, Key, El) end}
    end.

-spec chunk(iterator(BUP), fun((BUP) -> any())) -> iterator(list(BUP)).
chunk(Iterator, F) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                do_chunk(Next, F, F(E), E)
        end end,
    {iterator, _pipe}.

-spec next_sized_chunk(fun(() -> action(BUX)), integer(), list(BUX)) -> sized_chunk(BUX).
next_sized_chunk(Continuation, Left, Current_chunk) ->
    case Continuation() of
        stop ->
            case Current_chunk of
                [] ->
                    no_more;

                Remaining ->
                    {last, gleam@list:reverse(Remaining)}
            end;

        {continue, E, Next} ->
            Chunk = [E | Current_chunk],
            case Left > 1 of
                false ->
                    {another, gleam@list:reverse(Chunk), Next};

                true ->
                    next_sized_chunk(Next, Left - 1, Chunk)
            end
    end.

-spec do_sized_chunk(fun(() -> action(BVB)), integer()) -> fun(() -> action(list(BVB))).
do_sized_chunk(Continuation, Count) ->
    fun() -> case next_sized_chunk(Continuation, Count, []) of
            no_more ->
                stop;

            {last, Chunk} ->
                {continue, Chunk, fun stop/0};

            {another, Chunk@1, Next_element} ->
                {continue, Chunk@1, do_sized_chunk(Next_element, Count)}
        end end.

-spec sized_chunk(iterator(BVF), integer()) -> iterator(list(BVF)).
sized_chunk(Iterator, Count) ->
    _pipe = erlang:element(2, Iterator),
    _pipe@1 = do_sized_chunk(_pipe, Count),
    {iterator, _pipe@1}.

-spec do_intersperse(fun(() -> action(BVJ)), BVJ) -> action(BVJ).
do_intersperse(Continuation, Separator) ->
    case Continuation() of
        stop ->
            stop;

        {continue, E, Next} ->
            Next_interspersed = fun() -> do_intersperse(Next, Separator) end,
            {continue, Separator, fun() -> {continue, E, Next_interspersed} end}
    end.

-spec intersperse(iterator(BVM), BVM) -> iterator(BVM).
intersperse(Iterator, Elem) ->
    _pipe = fun() -> case (erlang:element(2, Iterator))() of
            stop ->
                stop;

            {continue, E, Next} ->
                {continue, E, fun() -> do_intersperse(Next, Elem) end}
        end end,
    {iterator, _pipe}.

-spec do_any(fun(() -> action(BVP)), fun((BVP) -> boolean())) -> boolean().
do_any(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            false;

        {continue, E, Next} ->
            Predicate(E) orelse do_any(Next, Predicate)
    end.

-spec any(iterator(BVR), fun((BVR) -> boolean())) -> boolean().
any(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    do_any(_pipe, Predicate).

-spec do_all(fun(() -> action(BVT)), fun((BVT) -> boolean())) -> boolean().
do_all(Continuation, Predicate) ->
    case Continuation() of
        stop ->
            true;

        {continue, E, Next} ->
            Predicate(E) andalso do_all(Next, Predicate)
    end.

-spec all(iterator(BVV), fun((BVV) -> boolean())) -> boolean().
all(Iterator, Predicate) ->
    _pipe = erlang:element(2, Iterator),
    do_all(_pipe, Predicate).

-spec update_group_with(BVX) -> fun((gleam@option:option(list(BVX))) -> list(BVX)).
update_group_with(El) ->
    fun(Maybe_group) -> case Maybe_group of
            {some, Group} ->
                [El | Group];

            none ->
                [El]
        end end.

-spec group_updater(fun((BWB) -> BWC)) -> fun((gleam@map:map_(BWC, list(BWB)), BWB) -> gleam@map:map_(BWC, list(BWB))).
group_updater(F) ->
    fun(Groups, Elem) ->
        _pipe = Groups,
        gleam@map:update(_pipe, F(Elem), update_group_with(Elem))
    end.

-spec group(iterator(BWJ), fun((BWJ) -> BWL)) -> gleam@map:map_(BWL, list(BWJ)).
group(Iterator, Key) ->
    _pipe = Iterator,
    _pipe@1 = fold(_pipe, gleam@map:new(), group_updater(Key)),
    gleam@map:map_values(
        _pipe@1,
        fun(_, Group) -> gleam@list:reverse(Group) end
    ).

-spec reduce(iterator(BWP), fun((BWP, BWP) -> BWP)) -> {ok, BWP} | {error, nil}.
reduce(Iterator, F) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, Next} ->
            _pipe = do_fold(Next, F, E),
            {ok, _pipe}
    end.

-spec last(iterator(BWT)) -> {ok, BWT} | {error, nil}.
last(Iterator) ->
    _pipe = Iterator,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-spec empty() -> iterator(any()).
empty() ->
    {iterator, fun stop/0}.

-spec once(fun(() -> BWZ)) -> iterator(BWZ).
once(F) ->
    _pipe = fun() -> {continue, F(), fun stop/0} end,
    {iterator, _pipe}.

-spec single(BXB) -> iterator(BXB).
single(Elem) ->
    once(fun() -> Elem end).

-spec do_interleave(fun(() -> action(BXD)), fun(() -> action(BXD))) -> action(BXD).
do_interleave(Current, Next) ->
    case Current() of
        stop ->
            Next();

        {continue, E, Next_other} ->
            {continue, E, fun() -> do_interleave(Next, Next_other) end}
    end.

-spec interleave(iterator(BXH), iterator(BXH)) -> iterator(BXH).
interleave(Left, Right) ->
    _pipe = fun() ->
        do_interleave(erlang:element(2, Left), erlang:element(2, Right))
    end,
    {iterator, _pipe}.

-spec do_fold_until(
    fun(() -> action(BXL)),
    fun((BXN, BXL) -> gleam@list:continue_or_stop(BXN)),
    BXN
) -> BXN.
do_fold_until(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            Accumulator;

        {continue, Elem, Next} ->
            case F(Accumulator, Elem) of
                {continue, Accumulator@1} ->
                    do_fold_until(Next, F, Accumulator@1);

                {stop, Accumulator@2} ->
                    Accumulator@2
            end
    end.

-spec fold_until(
    iterator(BXP),
    BXR,
    fun((BXR, BXP) -> gleam@list:continue_or_stop(BXR))
) -> BXR.
fold_until(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_fold_until(_pipe, F, Initial).

-spec do_try_fold(
    fun(() -> action(BXT)),
    fun((BXV, BXT) -> {ok, BXV} | {error, BXW}),
    BXV
) -> {ok, BXV} | {error, BXW}.
do_try_fold(Continuation, F, Accumulator) ->
    case Continuation() of
        stop ->
            {ok, Accumulator};

        {continue, Elem, Next} ->
            case F(Accumulator, Elem) of
                {error, _try} -> {error, _try};
                {ok, Accumulator@1} ->
                    do_try_fold(Next, F, Accumulator@1)
            end
    end.

-spec try_fold(iterator(BYB), BYD, fun((BYD, BYB) -> {ok, BYD} | {error, BYE})) -> {ok,
                                                                                    BYD} |
    {error, BYE}.
try_fold(Iterator, Initial, F) ->
    _pipe = erlang:element(2, Iterator),
    do_try_fold(_pipe, F, Initial).

-spec first(iterator(BYJ)) -> {ok, BYJ} | {error, nil}.
first(Iterator) ->
    case (erlang:element(2, Iterator))() of
        stop ->
            {error, nil};

        {continue, E, _@1} ->
            {ok, E}
    end.

-spec at(iterator(BYN), integer()) -> {ok, BYN} | {error, nil}.
at(Iterator, Index) ->
    _pipe = Iterator,
    _pipe@1 = drop(_pipe, Index),
    first(_pipe@1).
