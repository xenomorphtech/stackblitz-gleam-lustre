-module(gleam@map).
-compile(no_auto_import).

-export([size/1, to_list/1, from_list/1, has_key/2, new/0, get/2, insert/3, map_values/2, keys/1, values/1, filter/2, take/2, merge/2, delete/2, drop/2, update/3, fold/3]).
-export_type([map_/2]).

-type map_(Key, Value) :: any() | {gleam_phantom, Key, Value}.

-spec size(map_(any(), any())) -> integer().
size(Map) ->
    maps:size(Map).

-spec to_list(map_(ASS, AST)) -> list({ASS, AST}).
to_list(Map) ->
    maps:to_list(Map).

-spec from_list(list({ATC, ATD})) -> map_(ATC, ATD).
from_list(List) ->
    maps:from_list(List).

-spec has_key(map_(ATM, any()), ATM) -> boolean().
has_key(Map, Key) ->
    maps:is_key(Key, Map).

-spec new() -> map_(any(), any()).
new() ->
    maps:new().

-spec get(map_(AUC, AUD), AUC) -> {ok, AUD} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec insert(map_(AUO, AUP), AUO, AUP) -> map_(AUO, AUP).
insert(Map, Key, Value) ->
    maps:put(Key, Value, Map).

-spec map_values(map_(AVA, AVB), fun((AVA, AVB) -> AVE)) -> map_(AVA, AVE).
map_values(Map, Fun) ->
    maps:map(Fun, Map).

-spec keys(map_(AVO, any())) -> list(AVO).
keys(Map) ->
    maps:keys(Map).

-spec values(map_(any(), AVZ)) -> list(AVZ).
values(Map) ->
    maps:values(Map).

-spec filter(map_(AWI, AWJ), fun((AWI, AWJ) -> boolean())) -> map_(AWI, AWJ).
filter(Map, Property) ->
    maps:filter(Property, Map).

-spec take(map_(AWU, AWV), list(AWU)) -> map_(AWU, AWV).
take(Map, Desired_keys) ->
    maps:with(Desired_keys, Map).

-spec merge(map_(AXI, AXJ), map_(AXI, AXJ)) -> map_(AXI, AXJ).
merge(Map, New_entries) ->
    maps:merge(Map, New_entries).

-spec delete(map_(AXY, AXZ), AXY) -> map_(AXY, AXZ).
delete(Map, Key) ->
    maps:remove(Key, Map).

-spec drop(map_(AYK, AYL), list(AYK)) -> map_(AYK, AYL).
drop(Map, Disallowed_keys) ->
    gleam@list:fold(Disallowed_keys, Map, fun delete/2).

-spec update(map_(AYR, AYS), AYR, fun((gleam@option:option(AYS)) -> AYS)) -> map_(AYR, AYS).
update(Map, Key, Fun) ->
    _pipe = Map,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Map, Key, _pipe@3).

-spec do_fold(list({AYY, AYZ}), AZB, fun((AZB, AYY, AYZ) -> AZB)) -> AZB.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Tail] ->
            do_fold(Tail, Fun(Initial, K, V), Fun)
    end.

-spec fold(map_(AZC, AZD), AZG, fun((AZG, AZC, AZD) -> AZG)) -> AZG.
fold(Map, Initial, Fun) ->
    _pipe = Map,
    _pipe@1 = to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).
