-record(app, {
    init :: {any(), lustre@cmd:cmd(any())},
    update :: fun((any(), any()) -> {any(), lustre@cmd:cmd(any())}),
    render :: fun((any()) -> lustre@element:element(any()))
}).
