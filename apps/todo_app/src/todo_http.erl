-module(todo_http).
-export([json_reply/3]).

json_reply(Req0, Status, Term) ->
    Body0 = jsx:encode(Term),
    Body =
        case Body0 of
            {'incomplete', _Enc} ->
                error(incomplete_json_encoder);
            _ ->
                Body0
        end,
    cowboy_req:reply(
        Status,
        #{~b"content-type" => ~b"application/json"},
        Body,
        Req0
    ).
