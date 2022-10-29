-module(lustre@event).
-compile(no_auto_import).

-export([on/2, dispatch/1, on_click/1, on_mouse_down/1, on_mouse_up/1, on_mouse_enter/1, on_mouse_leave/1, on_mouse_over/1, on_mouse_out/1, on_keypress/1, on_keydown/1, on_keyup/1, on_input/1, on_check/1, on_submit/1, on_focus/1, on_blur/1]).

-spec on(binary(), fun((gleam@dynamic:dynamic(), fun((FOH) -> nil)) -> nil)) -> lustre@attribute:attribute(FOH).
on(Name, Handler) ->
    lustre@attribute:event(Name, Handler).

-spec dispatch(FOJ) -> fun((fun((FOJ) -> nil)) -> nil).
dispatch(Action) ->
    fun(Dispatch) -> Dispatch(Action) end.

-spec on_click(fun((fun((FOK) -> nil)) -> nil)) -> lustre@attribute:attribute(FOK).
on_click(Handler) ->
    on(<<"click"/utf8>>, fun(_, Dispatch) -> Handler(Dispatch) end).

-spec on_mouse_down(fun((fun((FOM) -> nil)) -> nil)) -> lustre@attribute:attribute(FOM).
on_mouse_down(Handler) ->
    on(<<"mouseDown"/utf8>>, fun(_, Dispatch) -> Handler(Dispatch) end).

-spec on_mouse_up(fun((fun((FOO) -> nil)) -> nil)) -> lustre@attribute:attribute(FOO).
on_mouse_up(Handler) ->
    on(<<"mouseUp"/utf8>>, fun(_, Dispatch) -> Handler(Dispatch) end).

-spec on_mouse_enter(fun((fun((FOQ) -> nil)) -> nil)) -> lustre@attribute:attribute(FOQ).
on_mouse_enter(Handler) ->
    on(<<"mouseEnter"/utf8>>, fun(_, Dispatch) -> Handler(Dispatch) end).

-spec on_mouse_leave(fun((fun((FOS) -> nil)) -> nil)) -> lustre@attribute:attribute(FOS).
on_mouse_leave(Handler) ->
    on(<<"mouseLeave"/utf8>>, fun(_, Dispatch) -> Handler(Dispatch) end).

-spec on_mouse_over(fun((fun((FOU) -> nil)) -> nil)) -> lustre@attribute:attribute(FOU).
on_mouse_over(Handler) ->
    on(<<"mouseOver"/utf8>>, fun(_, Dispatch) -> Handler(Dispatch) end).

-spec on_mouse_out(fun((fun((FOW) -> nil)) -> nil)) -> lustre@attribute:attribute(FOW).
on_mouse_out(Handler) ->
    on(<<"mouseOut"/utf8>>, fun(_, Dispatch) -> Handler(Dispatch) end).

-spec on_keypress(fun((binary(), fun((FOY) -> nil)) -> nil)) -> lustre@attribute:attribute(FOY).
on_keypress(Handler) ->
    on(
        <<"keyPress"/utf8>>,
        fun(E, Dispatch) ->
            {ok, Key@1} = case begin
                _pipe = E,
                (gleam@dynamic:field(<<"key"/utf8>>, fun gleam@dynamic:string/1))(
                    _pipe
                )
            end of
                {ok, Key} -> {ok, Key};
                _try ->
                    erlang:error(#{gleam_error => assert,
                                   message => <<"Assertion pattern match failed"/utf8>>,
                                   value => _try,
                                   module => <<"lustre/event"/utf8>>,
                                   function => <<"on_keypress"/utf8>>,
                                   line => 63})
            end,
            Handler(Key@1, Dispatch)
        end
    ).

-spec on_keydown(fun((binary(), fun((FPA) -> nil)) -> nil)) -> lustre@attribute:attribute(FPA).
on_keydown(Handler) ->
    on(
        <<"keyDown"/utf8>>,
        fun(E, Dispatch) ->
            {ok, Key@1} = case begin
                _pipe = E,
                (gleam@dynamic:field(<<"key"/utf8>>, fun gleam@dynamic:string/1))(
                    _pipe
                )
            end of
                {ok, Key} -> {ok, Key};
                _try ->
                    erlang:error(#{gleam_error => assert,
                                   message => <<"Assertion pattern match failed"/utf8>>,
                                   value => _try,
                                   module => <<"lustre/event"/utf8>>,
                                   function => <<"on_keydown"/utf8>>,
                                   line => 71})
            end,
            Handler(Key@1, Dispatch)
        end
    ).

-spec on_keyup(fun((binary(), fun((FPC) -> nil)) -> nil)) -> lustre@attribute:attribute(FPC).
on_keyup(Handler) ->
    on(
        <<"keyUp"/utf8>>,
        fun(E, Dispatch) ->
            {ok, Key@1} = case begin
                _pipe = E,
                (gleam@dynamic:field(<<"key"/utf8>>, fun gleam@dynamic:string/1))(
                    _pipe
                )
            end of
                {ok, Key} -> {ok, Key};
                _try ->
                    erlang:error(#{gleam_error => assert,
                                   message => <<"Assertion pattern match failed"/utf8>>,
                                   value => _try,
                                   module => <<"lustre/event"/utf8>>,
                                   function => <<"on_keyup"/utf8>>,
                                   line => 79})
            end,
            Handler(Key@1, Dispatch)
        end
    ).

-spec on_input(fun((binary(), fun((FPE) -> nil)) -> nil)) -> lustre@attribute:attribute(FPE).
on_input(Handler) ->
    on(
        <<"input"/utf8>>,
        fun(E, Dispatch) ->
            {ok, Value@1} = case begin
                _pipe = E,
                (gleam@dynamic:field(
                    <<"target"/utf8>>,
                    gleam@dynamic:field(
                        <<"value"/utf8>>,
                        fun gleam@dynamic:string/1
                    )
                ))(_pipe)
            end of
                {ok, Value} -> {ok, Value};
                _try ->
                    erlang:error(#{gleam_error => assert,
                                   message => <<"Assertion pattern match failed"/utf8>>,
                                   value => _try,
                                   module => <<"lustre/event"/utf8>>,
                                   function => <<"on_input"/utf8>>,
                                   line => 90})
            end,
            Handler(Value@1, Dispatch)
        end
    ).

-spec on_check(fun((boolean(), fun((FPG) -> nil)) -> nil)) -> lustre@attribute:attribute(FPG).
on_check(Handler) ->
    on(
        <<"check"/utf8>>,
        fun(E, Dispatch) ->
            {ok, Value@1} = case begin
                _pipe = E,
                (gleam@dynamic:field(
                    <<"target"/utf8>>,
                    gleam@dynamic:field(
                        <<"checked"/utf8>>,
                        fun gleam@dynamic:bool/1
                    )
                ))(_pipe)
            end of
                {ok, Value} -> {ok, Value};
                _try ->
                    erlang:error(#{gleam_error => assert,
                                   message => <<"Assertion pattern match failed"/utf8>>,
                                   value => _try,
                                   module => <<"lustre/event"/utf8>>,
                                   function => <<"on_check"/utf8>>,
                                   line => 98})
            end,
            Handler(Value@1, Dispatch)
        end
    ).

-spec on_submit(fun((fun((FPI) -> nil)) -> nil)) -> lustre@attribute:attribute(FPI).
on_submit(Handler) ->
    on(<<"submit"/utf8>>, fun(_, Dispatch) -> Handler(Dispatch) end).

-spec on_focus(fun((fun((FPK) -> nil)) -> nil)) -> lustre@attribute:attribute(FPK).
on_focus(Handler) ->
    on(<<"focus"/utf8>>, fun(_, Dispatch) -> Handler(Dispatch) end).

-spec on_blur(fun((fun((FPM) -> nil)) -> nil)) -> lustre@attribute:attribute(FPM).
on_blur(Handler) ->
    on(<<"blur"/utf8>>, fun(_, Dispatch) -> Handler(Dispatch) end).
