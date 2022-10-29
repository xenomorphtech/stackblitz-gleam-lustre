-record(event, {
    name :: binary(),
    handler :: fun((gleam@dynamic:dynamic(), fun((any()) -> nil)) -> nil)
}).
