-module(routing).
-export([routes/0]).

routes() ->
    [
        {'_', [{"/", qri_emitter, []}]}
    ].
