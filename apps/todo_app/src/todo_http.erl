-module(todo_http).
-export([json_reply/3, json_reply/4, json_error/3]).


json_reply(Req0, Status, Term) ->
    json_reply(Req0, Status, Term, #{}).

json_reply(Req0, Status, Term, ExtraHeaders) when is_map(ExtraHeaders) ->
    Body0 = jsx:encode(Term),
    Headers = maps:merge(
        #{~b"content-type" => ~b"application/json"},
        ExtraHeaders
    ),
    Body =
        case Body0 of
            {'incomplete', _Enc} ->
                error(incomplete_json_encoder);
            _ ->
                Body0
        end,
    cowboy_req:reply(
        Status,
        Headers,
        Body,
        Req0
    ).

json_error(Req0, Status, Message) ->
    json_reply(Req0, Status, #{~b"error" => Message}).
