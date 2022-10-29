-module(lustre).
-compile(no_auto_import).

-export([element/1, simple/3, application/3, start/2]).
-export_type([app/2, error/0]).

-opaque app(FSK, FSL) :: {app,
                          {FSK, lustre@cmd:cmd(FSL)},
                          fun((FSK, FSL) -> {FSK, lustre@cmd:cmd(FSL)}),
                          fun((FSK) -> lustre@element:element(FSL))}.

-type error() :: element_not_found.

-spec element(lustre@element:element(FTC)) -> app(nil, FTC).
element(Element) ->
    Init = {nil, lustre@cmd:none()},
    Update = fun(_, _) -> {nil, lustre@cmd:none()} end,
    Render = fun(_) -> Element end,
    {app, Init, Update, Render}.

-spec simple(
    FTG,
    fun((FTG, FTH) -> FTG),
    fun((FTG) -> lustre@element:element(FTH))
) -> app(FTG, FTH).
simple(Init, Update, Render) ->
    Init@1 = {Init, lustre@cmd:none()},
    Update@1 = fun(State, Action) ->
        {Update(State, Action), lustre@cmd:none()}
    end,
    {app, Init@1, Update@1, Render}.

-spec application(
    {FTL, lustre@cmd:cmd(FTM)},
    fun((FTL, FTM) -> {FTL, lustre@cmd:cmd(FTM)}),
    fun((FTL) -> lustre@element:element(FTM))
) -> app(FTL, FTM).
application(Init, Update, Render) ->
    {app, Init, Update, Render}.

-spec start(app(any(), FTV), binary()) -> {ok, fun((FTV) -> nil)} |
    {error, error()}.
start(App, Selector) ->
    _pipe = './ffi.mjs':mount(App, Selector),
    gleam@result:replace_error(_pipe, element_not_found).
