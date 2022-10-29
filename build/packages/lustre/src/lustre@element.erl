-module(lustre@element).
-compile(no_auto_import).

-export([node/3, stateful/2, fragment/1, text/1, map/2, html/3, base/1, head/2, meta/1, style/2, title/2, body/2, address/2, article/2, aside/2, footer/2, header/2, h1/2, h2/2, h3/2, h4/2, h5/2, h6/2, main/2, nav/2, section/2, blockquote/2, dd/2, 'div'/2, dl/2, dt/2, figcaption/2, figure/2, hr/1, li/2, menu/2, ol/2, p/2, pre/2, ul/2, a/2, abbr/2, b/2, bdi/2, bdo/2, br/1, cite/2, code/2, dfn/2, em/2, i/2, kbd/2, mark/2, rp/2, rt/2, ruby/2, s/2, samp/2, small/2, span/2, strong/2, sub/2, sup/2, time/2, u/2, var_/2, wbr/1, area/1, audio/2, img/1, map_/2, track/2, video/2, embed/1, iframe/1, object/2, param/2, picture/2, portal/1, source/1, svg/2, mathml/2, canvas/2, noscript/2, del/2, ins/2, caption/2, col/2, colgroup/2, table/2, tbody/2, td/2, tfoot/2, th/2, thead/2, tr/2, button/2, datalist/2, fieldset/2, form/2, input/1, label/2, legend/2, meter/2, optgroup/2, option/2, output/2, progress/2, select/2, textarea/1, details/2, dialog/2, summary/2, slot/2, template/2]).
-export_type([element/1]).

-type element(Action) :: any() | {gleam_phantom, Action}.

-spec node(binary(), list(lustre@attribute:attribute(EKD)), list(element(EKD))) -> element(EKD).
node(A, B, C) ->
    '../ffi.mjs':node(A, B, C).

-spec stateful(EKL, fun((EKL, fun((EKL) -> nil)) -> element(EKJ))) -> element(EKJ).
stateful(A, B) ->
    '../ffi.mjs':stateful(A, B).

-spec fragment(list(element(EKN))) -> element(EKN).
fragment(A) ->
    '../ffi.mjs':fragment(A).

-spec text(binary()) -> element(any()).
text(A) ->
    '../ffi.mjs':text(A).

-spec map(element(EKV), fun((EKV) -> EKT)) -> element(EKT).
map(A, B) ->
    '../ffi.mjs':map(A, B).

-spec html(list(lustre@attribute:attribute(EKX)), element(EKX), element(EKX)) -> element(EKX).
html(Attributes, Head, Body) ->
    '../ffi.mjs':node(<<"html"/utf8>>, Attributes, [Head, Body]).

-spec base(list(lustre@attribute:attribute(ELD))) -> element(ELD).
base(Attributes) ->
    '../ffi.mjs':node(<<"base"/utf8>>, Attributes, []).

-spec head(list(lustre@attribute:attribute(ELH)), list(element(ELH))) -> element(ELH).
head(Attributes, Children) ->
    '../ffi.mjs':node(<<"head"/utf8>>, Attributes, Children).

-spec meta(list(lustre@attribute:attribute(ELN))) -> element(ELN).
meta(Attributes) ->
    '../ffi.mjs':node(<<"meta"/utf8>>, Attributes, []).

-spec style(list(lustre@attribute:attribute(ELR)), binary()) -> element(ELR).
style(Attributes, Css) ->
    '../ffi.mjs':node(<<"style"/utf8>>, Attributes, ['../ffi.mjs':text(Css)]).

-spec title(list(lustre@attribute:attribute(ELV)), binary()) -> element(ELV).
title(Attributes, Name) ->
    '../ffi.mjs':node(<<"title"/utf8>>, Attributes, ['../ffi.mjs':text(Name)]).

-spec body(list(lustre@attribute:attribute(ELZ)), list(element(ELZ))) -> element(ELZ).
body(Attributes, Children) ->
    '../ffi.mjs':node(<<"body"/utf8>>, Attributes, Children).

-spec address(list(lustre@attribute:attribute(EMF)), list(element(EMF))) -> element(EMF).
address(Attributes, Children) ->
    '../ffi.mjs':node(<<"address"/utf8>>, Attributes, Children).

-spec article(list(lustre@attribute:attribute(EML)), list(element(EML))) -> element(EML).
article(Attributes, Children) ->
    '../ffi.mjs':node(<<"article"/utf8>>, Attributes, Children).

-spec aside(list(lustre@attribute:attribute(EMR)), list(element(EMR))) -> element(EMR).
aside(Attributes, Children) ->
    '../ffi.mjs':node(<<"aside"/utf8>>, Attributes, Children).

-spec footer(list(lustre@attribute:attribute(EMX)), list(element(EMX))) -> element(EMX).
footer(Attributes, Children) ->
    '../ffi.mjs':node(<<"footer"/utf8>>, Attributes, Children).

-spec header(list(lustre@attribute:attribute(END)), list(element(END))) -> element(END).
header(Attributes, Children) ->
    '../ffi.mjs':node(<<"header"/utf8>>, Attributes, Children).

-spec h1(list(lustre@attribute:attribute(ENJ)), list(element(ENJ))) -> element(ENJ).
h1(Attributes, Children) ->
    '../ffi.mjs':node(<<"h1"/utf8>>, Attributes, Children).

-spec h2(list(lustre@attribute:attribute(ENP)), list(element(ENP))) -> element(ENP).
h2(Attributes, Children) ->
    '../ffi.mjs':node(<<"h2"/utf8>>, Attributes, Children).

-spec h3(list(lustre@attribute:attribute(ENV)), list(element(ENV))) -> element(ENV).
h3(Attributes, Children) ->
    '../ffi.mjs':node(<<"h3"/utf8>>, Attributes, Children).

-spec h4(list(lustre@attribute:attribute(EOB)), list(element(EOB))) -> element(EOB).
h4(Attributes, Children) ->
    '../ffi.mjs':node(<<"h4"/utf8>>, Attributes, Children).

-spec h5(list(lustre@attribute:attribute(EOH)), list(element(EOH))) -> element(EOH).
h5(Attributes, Children) ->
    '../ffi.mjs':node(<<"h5"/utf8>>, Attributes, Children).

-spec h6(list(lustre@attribute:attribute(EON)), list(element(EON))) -> element(EON).
h6(Attributes, Children) ->
    '../ffi.mjs':node(<<"h6"/utf8>>, Attributes, Children).

-spec main(list(lustre@attribute:attribute(EOT)), list(element(EOT))) -> element(EOT).
main(Attributes, Children) ->
    '../ffi.mjs':node(<<"main"/utf8>>, Attributes, Children).

-spec nav(list(lustre@attribute:attribute(EOZ)), list(element(EOZ))) -> element(EOZ).
nav(Attributes, Children) ->
    '../ffi.mjs':node(<<"nav"/utf8>>, Attributes, Children).

-spec section(list(lustre@attribute:attribute(EPF)), list(element(EPF))) -> element(EPF).
section(Attributes, Children) ->
    '../ffi.mjs':node(<<"section"/utf8>>, Attributes, Children).

-spec blockquote(list(lustre@attribute:attribute(EPL)), list(element(EPL))) -> element(EPL).
blockquote(Attributes, Children) ->
    '../ffi.mjs':node(<<"blockquote"/utf8>>, Attributes, Children).

-spec dd(list(lustre@attribute:attribute(EPR)), list(element(EPR))) -> element(EPR).
dd(Attributes, Children) ->
    '../ffi.mjs':node(<<"dd"/utf8>>, Attributes, Children).

-spec 'div'(list(lustre@attribute:attribute(EPX)), list(element(EPX))) -> element(EPX).
'div'(Attributes, Children) ->
    '../ffi.mjs':node(<<"div"/utf8>>, Attributes, Children).

-spec dl(list(lustre@attribute:attribute(EQD)), list(element(EQD))) -> element(EQD).
dl(Attributes, Children) ->
    '../ffi.mjs':node(<<"dl"/utf8>>, Attributes, Children).

-spec dt(list(lustre@attribute:attribute(EQJ)), list(element(EQJ))) -> element(EQJ).
dt(Attributes, Children) ->
    '../ffi.mjs':node(<<"dt"/utf8>>, Attributes, Children).

-spec figcaption(list(lustre@attribute:attribute(EQP)), list(element(EQP))) -> element(EQP).
figcaption(Attributes, Children) ->
    '../ffi.mjs':node(<<"figcaption"/utf8>>, Attributes, Children).

-spec figure(list(lustre@attribute:attribute(EQV)), list(element(EQV))) -> element(EQV).
figure(Attributes, Children) ->
    '../ffi.mjs':node(<<"figure"/utf8>>, Attributes, Children).

-spec hr(list(lustre@attribute:attribute(ERB))) -> element(ERB).
hr(Attributes) ->
    '../ffi.mjs':node(<<"hr"/utf8>>, Attributes, []).

-spec li(list(lustre@attribute:attribute(ERF)), list(element(ERF))) -> element(ERF).
li(Attributes, Children) ->
    '../ffi.mjs':node(<<"li"/utf8>>, Attributes, Children).

-spec menu(list(lustre@attribute:attribute(ERL)), list(element(ERL))) -> element(ERL).
menu(Attributes, Children) ->
    '../ffi.mjs':node(<<"menu"/utf8>>, Attributes, Children).

-spec ol(list(lustre@attribute:attribute(ERR)), list(element(ERR))) -> element(ERR).
ol(Attributes, Children) ->
    '../ffi.mjs':node(<<"ol"/utf8>>, Attributes, Children).

-spec p(list(lustre@attribute:attribute(ERX)), list(element(ERX))) -> element(ERX).
p(Attributes, Children) ->
    '../ffi.mjs':node(<<"p"/utf8>>, Attributes, Children).

-spec pre(list(lustre@attribute:attribute(ESD)), list(element(ESD))) -> element(ESD).
pre(Attributes, Children) ->
    '../ffi.mjs':node(<<"pre"/utf8>>, Attributes, Children).

-spec ul(list(lustre@attribute:attribute(ESJ)), list(element(ESJ))) -> element(ESJ).
ul(Attributes, Children) ->
    '../ffi.mjs':node(<<"ul"/utf8>>, Attributes, Children).

-spec a(list(lustre@attribute:attribute(ESP)), list(element(ESP))) -> element(ESP).
a(Attributes, Children) ->
    '../ffi.mjs':node(<<"a"/utf8>>, Attributes, Children).

-spec abbr(list(lustre@attribute:attribute(ESV)), list(element(ESV))) -> element(ESV).
abbr(Attributes, Children) ->
    '../ffi.mjs':node(<<"abbr"/utf8>>, Attributes, Children).

-spec b(list(lustre@attribute:attribute(ETB)), list(element(ETB))) -> element(ETB).
b(Attributes, Children) ->
    '../ffi.mjs':node(<<"b"/utf8>>, Attributes, Children).

-spec bdi(list(lustre@attribute:attribute(ETH)), list(element(ETH))) -> element(ETH).
bdi(Attributes, Children) ->
    '../ffi.mjs':node(<<"bdi"/utf8>>, Attributes, Children).

-spec bdo(list(lustre@attribute:attribute(ETN)), list(element(ETN))) -> element(ETN).
bdo(Attributes, Children) ->
    '../ffi.mjs':node(<<"bdo"/utf8>>, Attributes, Children).

-spec br(list(lustre@attribute:attribute(ETT))) -> element(ETT).
br(Attributes) ->
    '../ffi.mjs':node(<<"br"/utf8>>, Attributes, []).

-spec cite(list(lustre@attribute:attribute(ETX)), list(element(ETX))) -> element(ETX).
cite(Attributes, Children) ->
    '../ffi.mjs':node(<<"cite"/utf8>>, Attributes, Children).

-spec code(list(lustre@attribute:attribute(EUD)), list(element(EUD))) -> element(EUD).
code(Attributes, Children) ->
    '../ffi.mjs':node(<<"code"/utf8>>, Attributes, Children).

-spec dfn(list(lustre@attribute:attribute(EUJ)), list(element(EUJ))) -> element(EUJ).
dfn(Attributes, Children) ->
    '../ffi.mjs':node(<<"dfn"/utf8>>, Attributes, Children).

-spec em(list(lustre@attribute:attribute(EUP)), list(element(EUP))) -> element(EUP).
em(Attributes, Children) ->
    '../ffi.mjs':node(<<"em"/utf8>>, Attributes, Children).

-spec i(list(lustre@attribute:attribute(EUV)), list(element(EUV))) -> element(EUV).
i(Attributes, Children) ->
    '../ffi.mjs':node(<<"i"/utf8>>, Attributes, Children).

-spec kbd(list(lustre@attribute:attribute(EVB)), list(element(EVB))) -> element(EVB).
kbd(Attributes, Children) ->
    '../ffi.mjs':node(<<"kbd"/utf8>>, Attributes, Children).

-spec mark(list(lustre@attribute:attribute(EVH)), list(element(EVH))) -> element(EVH).
mark(Attributes, Children) ->
    '../ffi.mjs':node(<<"mark"/utf8>>, Attributes, Children).

-spec rp(list(lustre@attribute:attribute(EVN)), list(element(EVN))) -> element(EVN).
rp(Attributes, Children) ->
    '../ffi.mjs':node(<<"rp"/utf8>>, Attributes, Children).

-spec rt(list(lustre@attribute:attribute(EVT)), list(element(EVT))) -> element(EVT).
rt(Attributes, Children) ->
    '../ffi.mjs':node(<<"rt"/utf8>>, Attributes, Children).

-spec ruby(list(lustre@attribute:attribute(EVZ)), list(element(EVZ))) -> element(EVZ).
ruby(Attributes, Children) ->
    '../ffi.mjs':node(<<"ruby"/utf8>>, Attributes, Children).

-spec s(list(lustre@attribute:attribute(EWF)), list(element(EWF))) -> element(EWF).
s(Attributes, Children) ->
    '../ffi.mjs':node(<<"s"/utf8>>, Attributes, Children).

-spec samp(list(lustre@attribute:attribute(EWL)), list(element(EWL))) -> element(EWL).
samp(Attributes, Children) ->
    '../ffi.mjs':node(<<"samp"/utf8>>, Attributes, Children).

-spec small(list(lustre@attribute:attribute(EWR)), list(element(EWR))) -> element(EWR).
small(Attributes, Children) ->
    '../ffi.mjs':node(<<"small"/utf8>>, Attributes, Children).

-spec span(list(lustre@attribute:attribute(EWX)), list(element(EWX))) -> element(EWX).
span(Attributes, Children) ->
    '../ffi.mjs':node(<<"span"/utf8>>, Attributes, Children).

-spec strong(list(lustre@attribute:attribute(EXD)), list(element(EXD))) -> element(EXD).
strong(Attributes, Children) ->
    '../ffi.mjs':node(<<"strong"/utf8>>, Attributes, Children).

-spec sub(list(lustre@attribute:attribute(EXJ)), list(element(EXJ))) -> element(EXJ).
sub(Attributes, Children) ->
    '../ffi.mjs':node(<<"sub"/utf8>>, Attributes, Children).

-spec sup(list(lustre@attribute:attribute(EXP)), list(element(EXP))) -> element(EXP).
sup(Attributes, Children) ->
    '../ffi.mjs':node(<<"sup"/utf8>>, Attributes, Children).

-spec time(list(lustre@attribute:attribute(EXV)), list(element(EXV))) -> element(EXV).
time(Attributes, Children) ->
    '../ffi.mjs':node(<<"time"/utf8>>, Attributes, Children).

-spec u(list(lustre@attribute:attribute(EYB)), list(element(EYB))) -> element(EYB).
u(Attributes, Children) ->
    '../ffi.mjs':node(<<"u"/utf8>>, Attributes, Children).

-spec var_(list(lustre@attribute:attribute(EYH)), list(element(EYH))) -> element(EYH).
var_(Attributes, Children) ->
    '../ffi.mjs':node(<<"var"/utf8>>, Attributes, Children).

-spec wbr(list(lustre@attribute:attribute(EYN))) -> element(EYN).
wbr(Attributes) ->
    '../ffi.mjs':node(<<"wbr"/utf8>>, Attributes, []).

-spec area(list(lustre@attribute:attribute(EYR))) -> element(EYR).
area(Attributes) ->
    '../ffi.mjs':node(<<"area"/utf8>>, Attributes, []).

-spec audio(list(lustre@attribute:attribute(EYV)), list(element(EYV))) -> element(EYV).
audio(Attributes, Children) ->
    '../ffi.mjs':node(<<"audio"/utf8>>, Attributes, Children).

-spec img(list(lustre@attribute:attribute(EZB))) -> element(EZB).
img(Attributes) ->
    '../ffi.mjs':node(<<"img"/utf8>>, Attributes, []).

-spec map_(list(lustre@attribute:attribute(EZF)), list(element(EZF))) -> element(EZF).
map_(Attributes, Children) ->
    '../ffi.mjs':node(<<"map"/utf8>>, Attributes, Children).

-spec track(list(lustre@attribute:attribute(EZL)), list(element(EZL))) -> element(EZL).
track(Attributes, Children) ->
    '../ffi.mjs':node(<<"track"/utf8>>, Attributes, Children).

-spec video(list(lustre@attribute:attribute(EZR)), list(element(EZR))) -> element(EZR).
video(Attributes, Children) ->
    '../ffi.mjs':node(<<"video"/utf8>>, Attributes, Children).

-spec embed(list(lustre@attribute:attribute(EZX))) -> element(EZX).
embed(Attributes) ->
    '../ffi.mjs':node(<<"embed"/utf8>>, Attributes, []).

-spec iframe(list(lustre@attribute:attribute(FAB))) -> element(FAB).
iframe(Attributes) ->
    '../ffi.mjs':node(<<"iframe"/utf8>>, Attributes, []).

-spec object(list(lustre@attribute:attribute(FAF)), list(element(FAF))) -> element(FAF).
object(Attributes, Children) ->
    '../ffi.mjs':node(<<"object"/utf8>>, Attributes, Children).

-spec param(list(lustre@attribute:attribute(FAL)), list(element(FAL))) -> element(FAL).
param(Attributes, Children) ->
    '../ffi.mjs':node(<<"param"/utf8>>, Attributes, Children).

-spec picture(list(lustre@attribute:attribute(FAR)), list(element(FAR))) -> element(FAR).
picture(Attributes, Children) ->
    '../ffi.mjs':node(<<"picture"/utf8>>, Attributes, Children).

-spec portal(list(lustre@attribute:attribute(FAX))) -> element(FAX).
portal(Attributes) ->
    '../ffi.mjs':node(<<"portal"/utf8>>, Attributes, []).

-spec source(list(lustre@attribute:attribute(FBB))) -> element(FBB).
source(Attributes) ->
    '../ffi.mjs':node(<<"source"/utf8>>, Attributes, []).

-spec svg(list(lustre@attribute:attribute(FBF)), list(element(FBF))) -> element(FBF).
svg(Attributes, Children) ->
    '../ffi.mjs':node(
        <<"svg"/utf8>>,
        [lustre@attribute:attribute(
             <<"xmlns"/utf8>>,
             <<"http://www.w3.org/2000/svg"/utf8>>
         ) |
         Attributes],
        Children
    ).

-spec mathml(list(lustre@attribute:attribute(FBL)), list(element(FBL))) -> element(FBL).
mathml(Attributes, Children) ->
    '../ffi.mjs':node(
        <<"mathml"/utf8>>,
        [lustre@attribute:attribute(
             <<"xmlns"/utf8>>,
             <<"http://www.w3.org/1998/Math/MathML"/utf8>>
         ) |
         Attributes],
        Children
    ).

-spec canvas(list(lustre@attribute:attribute(FBR)), list(element(FBR))) -> element(FBR).
canvas(Attributes, Children) ->
    '../ffi.mjs':node(<<"canvas"/utf8>>, Attributes, Children).

-spec noscript(list(lustre@attribute:attribute(FBX)), list(element(FBX))) -> element(FBX).
noscript(Attributes, Children) ->
    '../ffi.mjs':node(<<"noscript"/utf8>>, Attributes, Children).

-spec del(list(lustre@attribute:attribute(FCD)), list(element(FCD))) -> element(FCD).
del(Attributes, Children) ->
    '../ffi.mjs':node(<<"del"/utf8>>, Attributes, Children).

-spec ins(list(lustre@attribute:attribute(FCJ)), list(element(FCJ))) -> element(FCJ).
ins(Attributes, Children) ->
    '../ffi.mjs':node(<<"ins"/utf8>>, Attributes, Children).

-spec caption(list(lustre@attribute:attribute(FCP)), list(element(FCP))) -> element(FCP).
caption(Attributes, Children) ->
    '../ffi.mjs':node(<<"caption"/utf8>>, Attributes, Children).

-spec col(list(lustre@attribute:attribute(FCV)), list(element(FCV))) -> element(FCV).
col(Attributes, Children) ->
    '../ffi.mjs':node(<<"col"/utf8>>, Attributes, Children).

-spec colgroup(list(lustre@attribute:attribute(FDB)), list(element(FDB))) -> element(FDB).
colgroup(Attributes, Children) ->
    '../ffi.mjs':node(<<"colgroup"/utf8>>, Attributes, Children).

-spec table(list(lustre@attribute:attribute(FDH)), list(element(FDH))) -> element(FDH).
table(Attributes, Children) ->
    '../ffi.mjs':node(<<"table"/utf8>>, Attributes, Children).

-spec tbody(list(lustre@attribute:attribute(FDN)), list(element(FDN))) -> element(FDN).
tbody(Attributes, Children) ->
    '../ffi.mjs':node(<<"tbody"/utf8>>, Attributes, Children).

-spec td(list(lustre@attribute:attribute(FDT)), list(element(FDT))) -> element(FDT).
td(Attributes, Children) ->
    '../ffi.mjs':node(<<"td"/utf8>>, Attributes, Children).

-spec tfoot(list(lustre@attribute:attribute(FDZ)), list(element(FDZ))) -> element(FDZ).
tfoot(Attributes, Children) ->
    '../ffi.mjs':node(<<"tfoot"/utf8>>, Attributes, Children).

-spec th(list(lustre@attribute:attribute(FEF)), list(element(FEF))) -> element(FEF).
th(Attributes, Children) ->
    '../ffi.mjs':node(<<"th"/utf8>>, Attributes, Children).

-spec thead(list(lustre@attribute:attribute(FEL)), list(element(FEL))) -> element(FEL).
thead(Attributes, Children) ->
    '../ffi.mjs':node(<<"thead"/utf8>>, Attributes, Children).

-spec tr(list(lustre@attribute:attribute(FER)), list(element(FER))) -> element(FER).
tr(Attributes, Children) ->
    '../ffi.mjs':node(<<"tr"/utf8>>, Attributes, Children).

-spec button(list(lustre@attribute:attribute(FEX)), list(element(FEX))) -> element(FEX).
button(Attributes, Children) ->
    '../ffi.mjs':node(<<"button"/utf8>>, Attributes, Children).

-spec datalist(list(lustre@attribute:attribute(FFD)), list(element(FFD))) -> element(FFD).
datalist(Attributes, Children) ->
    '../ffi.mjs':node(<<"datalist"/utf8>>, Attributes, Children).

-spec fieldset(list(lustre@attribute:attribute(FFJ)), list(element(FFJ))) -> element(FFJ).
fieldset(Attributes, Children) ->
    '../ffi.mjs':node(<<"fieldset"/utf8>>, Attributes, Children).

-spec form(list(lustre@attribute:attribute(FFP)), list(element(FFP))) -> element(FFP).
form(Attributes, Children) ->
    '../ffi.mjs':node(<<"form"/utf8>>, Attributes, Children).

-spec input(list(lustre@attribute:attribute(FFV))) -> element(FFV).
input(Attributes) ->
    '../ffi.mjs':node(<<"input"/utf8>>, Attributes, []).

-spec label(list(lustre@attribute:attribute(FFZ)), list(element(FFZ))) -> element(FFZ).
label(Attributes, Children) ->
    '../ffi.mjs':node(<<"label"/utf8>>, Attributes, Children).

-spec legend(list(lustre@attribute:attribute(FGF)), list(element(FGF))) -> element(FGF).
legend(Attributes, Children) ->
    '../ffi.mjs':node(<<"legend"/utf8>>, Attributes, Children).

-spec meter(list(lustre@attribute:attribute(FGL)), list(element(FGL))) -> element(FGL).
meter(Attributes, Children) ->
    '../ffi.mjs':node(<<"meter"/utf8>>, Attributes, Children).

-spec optgroup(list(lustre@attribute:attribute(FGR)), list(element(FGR))) -> element(FGR).
optgroup(Attributes, Children) ->
    '../ffi.mjs':node(<<"optgroup"/utf8>>, Attributes, Children).

-spec option(list(lustre@attribute:attribute(FGX)), list(element(FGX))) -> element(FGX).
option(Attributes, Children) ->
    '../ffi.mjs':node(<<"option"/utf8>>, Attributes, Children).

-spec output(list(lustre@attribute:attribute(FHD)), list(element(FHD))) -> element(FHD).
output(Attributes, Children) ->
    '../ffi.mjs':node(<<"output"/utf8>>, Attributes, Children).

-spec progress(list(lustre@attribute:attribute(FHJ)), list(element(FHJ))) -> element(FHJ).
progress(Attributes, Children) ->
    '../ffi.mjs':node(<<"progress"/utf8>>, Attributes, Children).

-spec select(list(lustre@attribute:attribute(FHP)), list(element(FHP))) -> element(FHP).
select(Attributes, Children) ->
    '../ffi.mjs':node(<<"select"/utf8>>, Attributes, Children).

-spec textarea(list(lustre@attribute:attribute(FHV))) -> element(FHV).
textarea(Attributes) ->
    '../ffi.mjs':node(<<"textarea"/utf8>>, Attributes, []).

-spec details(list(lustre@attribute:attribute(FHZ)), list(element(FHZ))) -> element(FHZ).
details(Attributes, Children) ->
    '../ffi.mjs':node(<<"details"/utf8>>, Attributes, Children).

-spec dialog(list(lustre@attribute:attribute(FIF)), list(element(FIF))) -> element(FIF).
dialog(Attributes, Children) ->
    '../ffi.mjs':node(<<"dialog"/utf8>>, Attributes, Children).

-spec summary(list(lustre@attribute:attribute(FIL)), list(element(FIL))) -> element(FIL).
summary(Attributes, Children) ->
    '../ffi.mjs':node(<<"summary"/utf8>>, Attributes, Children).

-spec slot(list(lustre@attribute:attribute(FIR)), list(element(FIR))) -> element(FIR).
slot(Attributes, Children) ->
    '../ffi.mjs':node(<<"slot"/utf8>>, Attributes, Children).

-spec template(list(lustre@attribute:attribute(FIX)), list(element(FIX))) -> element(FIX).
template(Attributes, Children) ->
    '../ffi.mjs':node(<<"template"/utf8>>, Attributes, Children).
