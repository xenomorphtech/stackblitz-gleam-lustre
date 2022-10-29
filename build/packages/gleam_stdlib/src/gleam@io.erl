-module(gleam@io).
-compile(no_auto_import).

-export([print/1, println/1, debug/1]).

-spec print(binary()) -> nil.
print(String) ->
    gleam_stdlib:print(String).

-spec println(binary()) -> nil.
println(String) ->
    gleam_stdlib:println(String).

-spec debug(DZM) -> DZM.
debug(Term) ->
    _pipe = Term,
    _pipe@1 = gleam@string:inspect(_pipe),
    println(_pipe@1),
    Term.
