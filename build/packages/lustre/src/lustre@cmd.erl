-module(lustre@cmd).
-compile(no_auto_import).

-export([from/1, none/0, batch/1, map/2, to_list/1]).
-export_type([cmd/1]).

-opaque cmd(ECZ) :: {cmd, fun((fun((ECZ) -> nil)) -> nil), cmd(ECZ)} | none.

-spec from(fun((fun((EDA) -> nil)) -> nil)) -> cmd(EDA).
from(Cmd) ->
    {cmd, Cmd, none}.

-spec none() -> cmd(any()).
none() ->
    none.

-spec batch(list(cmd(EDE))) -> cmd(EDE).
batch(Cmds) ->
    _pipe = Cmds,
    _pipe@1 = gleam@list:flat_map(_pipe, fun to_list/1),
    gleam@list:fold_right(_pipe@1, none, fun(Rest, Cmd) -> {cmd, Cmd, Rest} end).

-spec map(cmd(EDI), fun((EDI) -> EDK)) -> cmd(EDK).
map(Cmd, F) ->
    case Cmd of
        {cmd, Cmd@1, Next} ->
            {cmd,
             fun(Dispatch) -> Cmd@1(fun(A) -> Dispatch(F(A)) end) end,
             map(Next, F)};

        none ->
            none
    end.

-spec to_list(cmd(EDM)) -> list(fun((fun((EDM) -> nil)) -> nil)).
to_list(Cmd) ->
    case Cmd of
        {cmd, Cmd@1, Next} ->
            [Cmd@1 | to_list(Next)];

        none ->
            []
    end.
