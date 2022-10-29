-module(lustre@attribute).
-compile(no_auto_import).

-export([attribute/2, property/2, event/2, style/1, class/1, classes/1, id/1, type_/1, value/1, checked/1, placeholder/1, selected/1, accept/1, accept_charset/1, action/1, autocomplete/1, autofocus/1, disabled/1, name/1, pattern/1, readonly/1, required/1, for/1, max/1, min/1, step/1, cols/1, rows/1, wrap/1, href/1, target/1, download/1, rel/1, src/1, height/1, width/1, alt/1, autoplay/1, controls/1, loop/1]).
-export_type([attribute/1]).

-opaque attribute(EEO) :: {attribute, binary(), binary()} |
    {property, binary(), gleam@dynamic:dynamic()} |
    {event, binary(), fun((gleam@dynamic:dynamic(), fun((EEO) -> nil)) -> nil)}.

-spec attribute(binary(), binary()) -> attribute(any()).
attribute(Name, Value) ->
    {attribute, Name, Value}.

-spec property(binary(), gleam@dynamic:dynamic()) -> attribute(any()).
property(Name, Value) ->
    {property, Name, Value}.

-spec event(binary(), fun((gleam@dynamic:dynamic(), fun((EET) -> nil)) -> nil)) -> attribute(EET).
event(Name, Handler) ->
    {event, Name, Handler}.

-spec style(list({binary(), binary()})) -> attribute(any()).
style(Properties) ->
    property(<<"style"/utf8>>, '../ffi.mjs':object(Properties)).

-spec class(binary()) -> attribute(any()).
class(Name) ->
    attribute(<<"className"/utf8>>, Name).

-spec classes(list({binary(), boolean()})) -> attribute(any()).
classes(Names) ->
    attribute(
        <<"className"/utf8>>,
        begin
            _pipe = Names,
            _pipe@1 = gleam@list:filter(_pipe, fun gleam@pair:second/1),
            _pipe@2 = gleam@list:map(_pipe@1, fun gleam@pair:first/1),
            gleam@string:join(_pipe@2, <<" "/utf8>>)
        end
    ).

-spec id(binary()) -> attribute(any()).
id(Name) ->
    attribute(<<"id"/utf8>>, Name).

-spec type_(binary()) -> attribute(any()).
type_(Name) ->
    attribute(<<"type"/utf8>>, Name).

-spec value(gleam@dynamic:dynamic()) -> attribute(any()).
value(Val) ->
    property(<<"value"/utf8>>, Val).

-spec checked(boolean()) -> attribute(any()).
checked(Is_checked) ->
    property(<<"checked"/utf8>>, gleam@dynamic:from(Is_checked)).

-spec placeholder(binary()) -> attribute(any()).
placeholder(Text) ->
    attribute(<<"placeholder"/utf8>>, Text).

-spec selected(boolean()) -> attribute(any()).
selected(Is_selected) ->
    property(<<"selected"/utf8>>, gleam@dynamic:from(Is_selected)).

-spec accept(list(binary())) -> attribute(any()).
accept(Types) ->
    attribute(<<"accept"/utf8>>, gleam@string:join(Types, <<" "/utf8>>)).

-spec accept_charset(list(binary())) -> attribute(any()).
accept_charset(Types) ->
    attribute(<<"acceptCharset"/utf8>>, gleam@string:join(Types, <<" "/utf8>>)).

-spec action(binary()) -> attribute(any()).
action(Uri) ->
    attribute(<<"action"/utf8>>, Uri).

-spec autocomplete(boolean()) -> attribute(any()).
autocomplete(Should_autocomplete) ->
    property(<<"autocomplete"/utf8>>, gleam@dynamic:from(Should_autocomplete)).

-spec autofocus(boolean()) -> attribute(any()).
autofocus(Should_autofocus) ->
    property(<<"autoFocus"/utf8>>, gleam@dynamic:from(Should_autofocus)).

-spec disabled(boolean()) -> attribute(any()).
disabled(Is_disabled) ->
    property(<<"disabled"/utf8>>, gleam@dynamic:from(Is_disabled)).

-spec name(binary()) -> attribute(any()).
name(Name) ->
    attribute(<<"name"/utf8>>, Name).

-spec pattern(binary()) -> attribute(any()).
pattern(Regex) ->
    attribute(<<"pattern"/utf8>>, Regex).

-spec readonly(boolean()) -> attribute(any()).
readonly(Is_readonly) ->
    property(<<"readonly"/utf8>>, gleam@dynamic:from(Is_readonly)).

-spec required(boolean()) -> attribute(any()).
required(Is_required) ->
    property(<<"required"/utf8>>, gleam@dynamic:from(Is_required)).

-spec for(binary()) -> attribute(any()).
for(Id) ->
    attribute(<<"for"/utf8>>, Id).

-spec max(binary()) -> attribute(any()).
max(Val) ->
    attribute(<<"max"/utf8>>, Val).

-spec min(binary()) -> attribute(any()).
min(Val) ->
    attribute(<<"min"/utf8>>, Val).

-spec step(binary()) -> attribute(any()).
step(Val) ->
    attribute(<<"step"/utf8>>, Val).

-spec cols(integer()) -> attribute(any()).
cols(Val) ->
    attribute(<<"cols"/utf8>>, gleam@int:to_string(Val)).

-spec rows(integer()) -> attribute(any()).
rows(Val) ->
    attribute(<<"rows"/utf8>>, gleam@int:to_string(Val)).

-spec wrap(binary()) -> attribute(any()).
wrap(Mode) ->
    attribute(<<"wrap"/utf8>>, Mode).

-spec href(binary()) -> attribute(any()).
href(Uri) ->
    attribute(<<"href"/utf8>>, Uri).

-spec target(binary()) -> attribute(any()).
target(Target) ->
    attribute(<<"target"/utf8>>, Target).

-spec download(binary()) -> attribute(any()).
download(Filename) ->
    attribute(<<"download"/utf8>>, Filename).

-spec rel(binary()) -> attribute(any()).
rel(Relationship) ->
    attribute(<<"rel"/utf8>>, Relationship).

-spec src(binary()) -> attribute(any()).
src(Uri) ->
    attribute(<<"src"/utf8>>, Uri).

-spec height(integer()) -> attribute(any()).
height(Val) ->
    attribute(<<"height"/utf8>>, gleam@int:to_string(Val)).

-spec width(integer()) -> attribute(any()).
width(Val) ->
    attribute(<<"width"/utf8>>, gleam@int:to_string(Val)).

-spec alt(binary()) -> attribute(any()).
alt(Text) ->
    attribute(<<"alt"/utf8>>, Text).

-spec autoplay(boolean()) -> attribute(any()).
autoplay(Should_autoplay) ->
    property(<<"autoplay"/utf8>>, gleam@dynamic:from(Should_autoplay)).

-spec controls(boolean()) -> attribute(any()).
controls(Visible) ->
    property(<<"controls"/utf8>>, gleam@dynamic:from(Visible)).

-spec loop(boolean()) -> attribute(any()).
loop(Should_loop) ->
    property(<<"loop"/utf8>>, gleam@dynamic:from(Should_loop)).
