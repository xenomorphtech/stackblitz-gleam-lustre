-module(gleam@list).
-compile(no_auto_import).

-export([length/1, reverse/1, is_empty/1, contains/2, first/1, rest/1, filter/2, filter_map/2, map/2, map_fold/3, index_map/2, try_map/2, drop/2, take/2, new/0, append/2, prepend/2, flatten/1, flat_map/2, fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, at/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, partition/2, permutations/1, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, interleave/1, transpose/1]).
-export_type([length_mismatch/0, continue_or_stop/1]).

-type length_mismatch() :: length_mismatch.

-type continue_or_stop(EZ) :: {continue, EZ} | {stop, EZ}.

-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-spec reverse(list(FE)) -> list(FE).
reverse(Xs) ->
    lists:reverse(Xs).

-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-spec contains(list(FM), FM) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [Head | _@1] when Head =:= Elem ->
            true;

        [_@2 | Tail] ->
            contains(Tail, Elem)
    end.

-spec first(list(FO)) -> {ok, FO} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _@1] ->
            {ok, X}
    end.

-spec rest(list(FS)) -> {ok, list(FS)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_@1 | Xs] ->
            {ok, Xs}
    end.

-spec do_filter(list(FX), fun((FX) -> boolean()), list(FX)) -> list(FX).
do_filter(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                true ->
                    [X | Acc];

                false ->
                    Acc
            end,
            do_filter(Xs, Fun, New_acc)
    end.

-spec filter(list(GB), fun((GB) -> boolean())) -> list(GB).
filter(List, Predicate) ->
    do_filter(List, Predicate, []).

-spec do_filter_map(list(GE), fun((GE) -> {ok, GG} | {error, any()}), list(GG)) -> list(GG).
do_filter_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                {ok, X@1} ->
                    [X@1 | Acc];

                {error, _@1} ->
                    Acc
            end,
            do_filter_map(Xs, Fun, New_acc)
    end.

-spec filter_map(list(GM), fun((GM) -> {ok, GO} | {error, any()})) -> list(GO).
filter_map(List, Fun) ->
    do_filter_map(List, Fun, []).

-spec do_map(list(GT), fun((GT) -> GV), list(GV)) -> list(GV).
do_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            do_map(Xs, Fun, [Fun(X) | Acc])
    end.

-spec map(list(GY), fun((GY) -> HA)) -> list(HA).
map(List, Fun) ->
    do_map(List, Fun, []).

-spec map_fold(list(HC), HE, fun((HE, HC) -> {HE, HF})) -> {HE, list(HF)}.
map_fold(List, Acc, Fun) ->
    _pipe = fold(
        List,
        {Acc, []},
        fun(Acc@1, Item) ->
            {Current_acc, Items} = Acc@1,
            {Next_acc, Next_item} = Fun(Current_acc, Item),
            {Next_acc, [Next_item | Items]}
        end
    ),
    gleam@pair:map_second(_pipe, fun reverse/1).

-spec do_index_map(list(HH), fun((integer(), HH) -> HJ), integer(), list(HJ)) -> list(HJ).
do_index_map(List, Fun, Index, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            Acc@1 = [Fun(Index, X) | Acc],
            do_index_map(Xs, Fun, Index + 1, Acc@1)
    end.

-spec index_map(list(HM), fun((integer(), HM) -> HO)) -> list(HO).
index_map(List, Fun) ->
    do_index_map(List, Fun, 0, []).

-spec do_try_map(list(HQ), fun((HQ) -> {ok, HS} | {error, HT}), list(HS)) -> {ok,
                                                                              list(HS)} |
    {error, HT}.
do_try_map(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, reverse(Acc)};

        [X | Xs] ->
            case Fun(X) of
                {ok, Y} ->
                    do_try_map(Xs, Fun, [Y | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec try_map(list(IA), fun((IA) -> {ok, IC} | {error, ID})) -> {ok, list(IC)} |
    {error, ID}.
try_map(List, Fun) ->
    do_try_map(List, Fun, []).

-spec drop(list(IJ), integer()) -> list(IJ).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_@1 | Xs] ->
                    drop(Xs, N - 1)
            end
    end.

-spec do_take(list(IM), integer(), list(IM)) -> list(IM).
do_take(List, N, Acc) ->
    case N =< 0 of
        true ->
            reverse(Acc);

        false ->
            case List of
                [] ->
                    reverse(Acc);

                [X | Xs] ->
                    do_take(Xs, N - 1, [X | Acc])
            end
    end.

-spec take(list(IQ), integer()) -> list(IQ).
take(List, N) ->
    do_take(List, N, []).

-spec new() -> list(any()).
new() ->
    [].

-spec append(list(IV), list(IV)) -> list(IV).
append(First, Second) ->
    lists:append(First, Second).

-spec prepend(list(JD), JD) -> list(JD).
prepend(List, Item) ->
    [Item | List].

-spec do_flatten(list(list(JG)), list(JG)) -> list(JG).
do_flatten(Lists, Acc) ->
    case Lists of
        [] ->
            Acc;

        [L | Rest] ->
            do_flatten(Rest, append(Acc, L))
    end.

-spec flatten(list(list(JL))) -> list(JL).
flatten(Lists) ->
    do_flatten(Lists, []).

-spec flat_map(list(JP), fun((JP) -> list(JR))) -> list(JR).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    flatten(_pipe).

-spec fold(list(JU), JW, fun((JW, JU) -> JW)) -> JW.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-spec fold_right(list(JX), JZ, fun((JZ, JX) -> JZ)) -> JZ.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-spec do_index_fold(list(KA), KC, fun((KC, KA, integer()) -> KC), integer()) -> KC.
do_index_fold(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            do_index_fold(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-spec index_fold(list(KD), KF, fun((KF, KD, integer()) -> KF)) -> KF.
index_fold(Over, Initial, Fun) ->
    do_index_fold(Over, Initial, Fun, 0).

-spec try_fold(list(KG), KI, fun((KI, KG) -> {ok, KI} | {error, KJ})) -> {ok,
                                                                          KI} |
    {error, KJ}.
try_fold(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            {ok, Accumulator};

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {error, _try} -> {error, _try};
                {ok, Accumulator@1} ->
                    try_fold(Rest, Accumulator@1, Fun)
            end
    end.

-spec fold_until(list(KO), KQ, fun((KQ, KO) -> continue_or_stop(KQ))) -> KQ.
fold_until(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            Accumulator;

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-spec find(list(KS), fun((KS) -> boolean())) -> {ok, KS} | {error, nil}.
find(Haystack, Is_desired) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Is_desired(X) of
                true ->
                    {ok, X};

                _@1 ->
                    find(Rest, Is_desired)
            end
    end.

-spec find_map(list(KW), fun((KW) -> {ok, KY} | {error, any()})) -> {ok, KY} |
    {error, nil}.
find_map(Haystack, Fun) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X@1} ->
                    {ok, X@1};

                _@1 ->
                    find_map(Rest, Fun)
            end
    end.

-spec all(list(LE), fun((LE) -> boolean())) -> boolean().
all(List, Predicate) ->
    case List of
        [] ->
            true;

        [Head | Tail] ->
            case Predicate(Head) of
                true ->
                    all(Tail, Predicate);

                false ->
                    false
            end
    end.

-spec any(list(LG), fun((LG) -> boolean())) -> boolean().
any(List, Predicate) ->
    case List of
        [] ->
            false;

        [Head | Tail] ->
            case Predicate(Head) of
                true ->
                    true;

                false ->
                    any(Tail, Predicate)
            end
    end.

-spec do_zip(list(LI), list(LK), list({LI, LK})) -> list({LI, LK}).
do_zip(Xs, Ys, Acc) ->
    case {Xs, Ys} of
        {[X | Xs@1], [Y | Ys@1]} ->
            do_zip(Xs@1, Ys@1, [{X, Y} | Acc]);

        {_@1, _@2} ->
            reverse(Acc)
    end.

-spec zip(list(LO), list(LQ)) -> list({LO, LQ}).
zip(Xs, Ys) ->
    do_zip(Xs, Ys, []).

-spec strict_zip(list(LT), list(LV)) -> {ok, list({LT, LV})} |
    {error, length_mismatch()}.
strict_zip(L1, L2) ->
    case length(L1) =:= length(L2) of
        true ->
            {ok, zip(L1, L2)};

        false ->
            {error, length_mismatch}
    end.

-spec do_unzip(list({ME, MF}), list(ME), list(MF)) -> {list(ME), list(MF)}.
do_unzip(Input, Xs, Ys) ->
    case Input of
        [] ->
            {reverse(Xs), reverse(Ys)};

        [{X, Y} | Rest] ->
            do_unzip(Rest, [X | Xs], [Y | Ys])
    end.

-spec unzip(list({ME, MF})) -> {list(ME), list(MF)}.
unzip(Input) ->
    do_unzip(Input, [], []).

-spec do_intersperse(list(MJ), MJ, list(MJ)) -> list(MJ).
do_intersperse(List, Separator, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Rest] ->
            do_intersperse(Rest, Separator, [X, Separator | Acc])
    end.

-spec intersperse(list(MN), MN) -> list(MN).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_@1] ->
            List;

        [X | Rest] ->
            do_intersperse(Rest, Elem, [X])
    end.

-spec at(list(MQ), integer()) -> {ok, MQ} | {error, nil}.
at(List, Index) ->
    _pipe = List,
    _pipe@1 = drop(_pipe, Index),
    first(_pipe@1).

-spec unique(list(MU)) -> list(MU).
unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

-spec do_merge_sort(list(MX), list(MX), fun((MX, MX) -> gleam@order:order())) -> list(MX).
do_merge_sort(A, B, Compare) ->
    case {A, B} of
        {[], _@1} ->
            B;

        {_@2, []} ->
            A;

        {[Ax | Ar], [Bx | Br]} ->
            case Compare(Ax, Bx) of
                lt ->
                    [Ax | do_merge_sort(Ar, B, Compare)];

                _@3 ->
                    [Bx | do_merge_sort(A, Br, Compare)]
            end
    end.

-spec do_sort(list(NB), fun((NB, NB) -> gleam@order:order()), integer()) -> list(NB).
do_sort(List, Compare, List_length) ->
    case List_length < 2 of
        true ->
            List;

        false ->
            Split_length = List_length div 2,
            A_list = take(List, Split_length),
            B_list = drop(List, Split_length),
            do_merge_sort(
                do_sort(A_list, Compare, Split_length),
                do_sort(B_list, Compare, List_length - Split_length),
                Compare
            )
    end.

-spec sort(list(NE), fun((NE, NE) -> gleam@order:order())) -> list(NE).
sort(List, Compare) ->
    do_sort(List, Compare, length(List)).

-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    tail_recursive_range(Start, Stop, []).

-spec tail_recursive_range(integer(), integer(), list(integer())) -> list(integer()).
tail_recursive_range(Start, Stop, Acc) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            reverse([Stop | Acc]);

        gt ->
            tail_recursive_range(Start - 1, Stop, [Start | Acc]);

        lt ->
            tail_recursive_range(Start + 1, Stop, [Start | Acc])
    end.

-spec do_repeat(NK, integer(), list(NK)) -> list(NK).
do_repeat(A, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            do_repeat(A, Times - 1, [A | Acc])
    end.

-spec repeat(NN, integer()) -> list(NN).
repeat(A, Times) ->
    do_repeat(A, Times, []).

-spec do_split(list(NP), integer(), list(NP)) -> {list(NP), list(NP)}.
do_split(List, N, Taken) ->
    case N =< 0 of
        true ->
            {reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {reverse(Taken), []};

                [X | Xs] ->
                    do_split(Xs, N - 1, [X | Taken])
            end
    end.

-spec split(list(NU), integer()) -> {list(NU), list(NU)}.
split(List, Index) ->
    do_split(List, Index, []).

-spec do_split_while(list(NY), fun((NY) -> boolean()), list(NY)) -> {list(NY),
                                                                     list(NY)}.
do_split_while(List, F, Acc) ->
    case List of
        [] ->
            {reverse(Acc), []};

        [X | Xs] ->
            case F(X) of
                false ->
                    {reverse(Acc), List};

                _@1 ->
                    do_split_while(Xs, F, [X | Acc])
            end
    end.

-spec split_while(list(OD), fun((OD) -> boolean())) -> {list(OD), list(OD)}.
split_while(List, Predicate) ->
    do_split_while(List, Predicate, []).

-spec key_find(list({OH, OI}), OH) -> {ok, OI} | {error, nil}.
key_find(Keyword_list, Desired_key) ->
    find_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-spec do_pop(list(OQ), fun((OQ) -> boolean()), list(OQ)) -> {ok, {OQ, list(OQ)}} |
    {error, nil}.
do_pop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, append(reverse(Checked), Rest)}};

                false ->
                    do_pop(Rest, Predicate, [X | Checked])
            end
    end.

-spec pop(list(OQ), fun((OQ) -> boolean())) -> {ok, {OQ, list(OQ)}} |
    {error, nil}.
pop(Haystack, Is_desired) ->
    do_pop(Haystack, Is_desired, []).

-spec do_pop_map(list(OZ), fun((OZ) -> {ok, PB} | {error, any()}), list(OZ)) -> {ok,
                                                                                 {PB,
                                                                                  list(OZ)}} |
    {error, nil}.
do_pop_map(Haystack, Mapper, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, append(reverse(Checked), Rest)}};

                {error, _@1} ->
                    do_pop_map(Rest, Mapper, [X | Checked])
            end
    end.

-spec pop_map(list(OZ), fun((OZ) -> {ok, PB} | {error, any()})) -> {ok,
                                                                    {PB,
                                                                     list(OZ)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    do_pop_map(Haystack, Is_desired, []).

-spec key_pop(list({PI, PJ}), PI) -> {ok, {PJ, list({PI, PJ})}} | {error, nil}.
key_pop(Haystack, Key) ->
    pop_map(
        Haystack,
        fun(Entry) ->
            {K, V} = Entry,
            case K of
                K@1 when K@1 =:= Key ->
                    {ok, V};

                _@1 ->
                    {error, nil}
            end
        end
    ).

-spec key_set(list({PO, PP}), PO, PP) -> list({PO, PP}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _@1} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-spec each(list(PS), fun((PS) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [X | Xs] ->
            F(X),
            each(Xs, F)
    end.

-spec do_partition(list(QA), fun((QA) -> boolean()), list(QA), list(QA)) -> {list(QA),
                                                                             list(QA)}.
do_partition(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {reverse(Trues), reverse(Falses)};

        [X | Xs] ->
            case Categorise(X) of
                true ->
                    do_partition(Xs, Categorise, [X | Trues], Falses);

                false ->
                    do_partition(Xs, Categorise, Trues, [X | Falses])
            end
    end.

-spec partition(list(QA), fun((QA) -> boolean())) -> {list(QA), list(QA)}.
partition(List, Categorise) ->
    do_partition(List, Categorise, [], []).

-spec permutations(list(QE)) -> list(list(QE)).
permutations(L) ->
    case L of
        [] ->
            [[]];

        _@1 ->
            _pipe@2 = map(
                L,
                fun(X) ->
                    _pipe = filter(L, fun(Y) -> Y /= X end),
                    _pipe@1 = permutations(_pipe),
                    map(_pipe@1, fun(_capture) -> append([X], _capture) end)
                end
            ),
            flatten(_pipe@2)
    end.

-spec do_window(list(list(QI)), list(QI), integer()) -> list(list(QI)).
do_window(Acc, L, N) ->
    Window = take(L, N),
    case length(Window) =:= N of
        true ->
            do_window([Window | Acc], drop(L, 1), N);

        false ->
            Acc
    end.

-spec window(list(QO), integer()) -> list(list(QO)).
window(L, N) ->
    _pipe = do_window([], L, N),
    reverse(_pipe).

-spec window_by_2(list(QS)) -> list({QS, QS}).
window_by_2(L) ->
    zip(L, drop(L, 1)).

-spec drop_while(list(QV), fun((QV) -> boolean())) -> list(QV).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [X | Xs] ->
            case Predicate(X) of
                true ->
                    drop_while(Xs, Predicate);

                false ->
                    [X | Xs]
            end
    end.

-spec do_take_while(list(QY), fun((QY) -> boolean()), list(QY)) -> list(QY).
do_take_while(List, Predicate, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [Head | Tail] ->
            case Predicate(Head) of
                true ->
                    do_take_while(Tail, Predicate, [Head | Acc]);

                false ->
                    reverse(Acc)
            end
    end.

-spec take_while(list(RC), fun((RC) -> boolean())) -> list(RC).
take_while(List, Predicate) ->
    do_take_while(List, Predicate, []).

-spec do_chunk(list(RF), fun((RF) -> RH), RH, list(RF), list(list(RF))) -> list(list(RF)).
do_chunk(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [Head | Tail] ->
            Key = F(Head),
            case Key =:= Previous_key of
                false ->
                    New_acc = [reverse(Current_chunk) | Acc],
                    do_chunk(Tail, F, Key, [Head], New_acc);

                _@1 ->
                    do_chunk(Tail, F, Key, [Head | Current_chunk], Acc)
            end;

        _@2 ->
            reverse([reverse(Current_chunk) | Acc])
    end.

-spec chunk(list(RN), fun((RN) -> any())) -> list(list(RN)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [Head | Tail] ->
            do_chunk(Tail, F, F(Head), [Head], [])
    end.

-spec do_sized_chunk(list(RS), integer(), integer(), list(RS), list(list(RS))) -> list(list(RS)).
do_sized_chunk(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    reverse(Acc);

                Remaining ->
                    reverse([reverse(Remaining) | Acc])
            end;

        [Head | Tail] ->
            Chunk = [Head | Current_chunk],
            case Left > 1 of
                false ->
                    do_sized_chunk(
                        Tail,
                        Count,
                        Count,
                        [],
                        [reverse(Chunk) | Acc]
                    );

                true ->
                    do_sized_chunk(Tail, Count, Left - 1, Chunk, Acc)
            end
    end.

-spec sized_chunk(list(RZ), integer()) -> list(list(RZ)).
sized_chunk(List, Count) ->
    do_sized_chunk(List, Count, Count, [], []).

-spec reduce(list(SD), fun((SD, SD) -> SD)) -> {ok, SD} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [Head | Tail] ->
            {ok, fold(Tail, Head, Fun)}
    end.

-spec do_scan(list(SH), SJ, list(SJ), fun((SJ, SH) -> SJ)) -> list(SJ).
do_scan(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            reverse(Accumulated);

        [X | Xs] ->
            Next = Fun(Accumulator, X),
            do_scan(Xs, Next, [Next | Accumulated], Fun)
    end.

-spec scan(list(SM), SO, fun((SO, SM) -> SO)) -> list(SO).
scan(List, Initial, Fun) ->
    do_scan(List, Initial, [], Fun).

-spec last(list(SQ)) -> {ok, SQ} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-spec combinations(list(SU), integer()) -> list(list(SU)).
combinations(Items, N) ->
    case N of
        0 ->
            [[]];

        _@1 ->
            case Items of
                [] ->
                    [];

                [X | Xs] ->
                    First_combinations = begin
                        _pipe = map(
                            combinations(Xs, N - 1),
                            fun(Com) -> [X | Com] end
                        ),
                        reverse(_pipe)
                    end,
                    fold(
                        First_combinations,
                        combinations(Xs, N),
                        fun(Acc, C) -> [C | Acc] end
                    )
            end
    end.

-spec do_combination_pairs(list(SY)) -> list(list({SY, SY})).
do_combination_pairs(Items) ->
    case Items of
        [] ->
            [];

        [X | Xs] ->
            First_combinations = map(Xs, fun(Other) -> {X, Other} end),
            [First_combinations | do_combination_pairs(Xs)]
    end.

-spec combination_pairs(list(TC)) -> list({TC, TC}).
combination_pairs(Items) ->
    _pipe = do_combination_pairs(Items),
    flatten(_pipe).

-spec interleave(list(list(TF))) -> list(TF).
interleave(List) ->
    _pipe = transpose(List),
    flatten(_pipe).

-spec transpose(list(list(TJ))) -> list(list(TJ)).
transpose(List_of_list) ->
    Take_first = fun(List) -> case List of
            [] ->
                [];

            [F] ->
                [F];

            [F@1 | _@1] ->
                [F@1]
        end end,
    case List_of_list of
        [] ->
            [];

        [[] | Xss] ->
            transpose(Xss);

        Rows ->
            Firsts = begin
                _pipe = Rows,
                _pipe@1 = map(_pipe, Take_first),
                flatten(_pipe@1)
            end,
            Rest = transpose(map(Rows, fun(_capture) -> drop(_capture, 1) end)),
            [Firsts | Rest]
    end.
