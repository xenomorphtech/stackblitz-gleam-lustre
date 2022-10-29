-module(gleam@pair).
-compile(no_auto_import).

-export([first/1, second/1, swap/1, map_first/2, map_second/2]).

-spec first({EM, any()}) -> EM.
first(Pair) ->
    {A, _@1} = Pair,
    A.

-spec second({any(), EP}) -> EP.
second(Pair) ->
    {_@1, A} = Pair,
    A.

-spec swap({EQ, ER}) -> {ER, EQ}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-spec map_first({ES, ET}, fun((ES) -> EU)) -> {EU, ET}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-spec map_second({EV, EW}, fun((EW) -> EX)) -> {EV, EX}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.
