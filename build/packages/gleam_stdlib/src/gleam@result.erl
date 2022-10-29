-module(gleam@result).
-compile(no_auto_import).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, replace/2, replace_error/2, values/1]).

-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _@1} ->
            false;

        {ok, _@2} ->
            true
    end.

-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _@1} ->
            false;

        {error, _@2} ->
            true
    end.

-spec map({ok, BBQ} | {error, BBR}, fun((BBQ) -> BBU)) -> {ok, BBU} |
    {error, BBR}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BBX} | {error, BBY}, fun((BBY) -> BCB)) -> {ok, BBX} |
    {error, BCB}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BCE} | {error, BCF}} | {error, BCF}) -> {ok, BCE} |
    {error, BCF}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec then({ok, BCM} | {error, BCN}, fun((BCM) -> {ok, BCQ} | {error, BCN})) -> {ok,
                                                                                 BCQ} |
    {error, BCN}.
then(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec unwrap({ok, BCV} | {error, any()}, BCV) -> BCV.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _@1} ->
            Default
    end.

-spec lazy_unwrap({ok, BCZ} | {error, any()}, fun(() -> BCZ)) -> BCZ.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _@1} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, BDE}, BDE) -> BDE.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _@1} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, BDH} | {error, BDH}) -> BDH.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, BDK} | {error, any()}) -> {ok, BDK} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, BDQ} | {error, BDR}, {ok, BDQ} | {error, BDR}) -> {ok, BDQ} |
    {error, BDR}.
'or'(First, Second) ->
    case First of
        {ok, _@1} ->
            First;

        {error, _@2} ->
            Second
    end.

-spec lazy_or({ok, BDY} | {error, BDZ}, fun(() -> {ok, BDY} | {error, BDZ})) -> {ok,
                                                                                 BDY} |
    {error, BDZ}.
lazy_or(First, Second) ->
    case First of
        {ok, _@1} ->
            First;

        {error, _@2} ->
            Second()
    end.

-spec all(list({ok, BEG} | {error, BEH})) -> {ok, list(BEG)} | {error, BEH}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec replace({ok, any()} | {error, BEP}, BES) -> {ok, BES} | {error, BEP}.
replace(Result, Value) ->
    case Result of
        {ok, _@1} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, BEV} | {error, any()}, BEZ) -> {ok, BEV} | {error, BEZ}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _@1} ->
            {error, Error}
    end.

-spec values(list({ok, BFC} | {error, any()})) -> list(BFC).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).
