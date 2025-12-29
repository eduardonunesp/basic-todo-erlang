-module(todo_health_handler).

-export([init/2]).

init(Req0, State) ->
    Req =
        todo_http:json_reply(
            Req0,
            200,
            #{~b"status" => ~b"ok"}
        ),
    {ok, Req, State}.
