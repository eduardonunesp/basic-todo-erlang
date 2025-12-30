-module(todo_todos_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case parse_id(Req0) of
        {error, ReqBad} ->
            {ok, ReqBad, State};

        {no_id, Req1} ->
            handle(Method, no_id, Req1, State);

        {ok, Id, Req1} ->
            handle(Method, Id, Req1, State)
    end.

handle(~b"GET", no_id, Req0, State) ->
    Todos = todo_store:list(),
    Req = todo_http:json_reply(Req0, 200, Todos),
    {ok, Req, State};

handle(~b"GET", Id, Req0, State) ->
    case todo_store:get(Id) of
        {ok, Todo} ->
            Req = todo_http:json_reply(Req0, 200, Todo),
            {ok, Req, State};
        not_found ->
            Req = todo_http:json_error(Req0, 404, ~b"not_found"),
            {ok, Req, State}
    end;

handle(~b"DELETE", Id, Req0, State) ->
    case todo_store:delete(Id) of
        ok ->
            Req = todo_http:json_reply(Req0, 200, #{~b"deleted" => true}),
            {ok, Req, State};
        not_found ->
            Req = todo_http:json_error(Req0, 404, ~b"not_found"),
            {ok, Req, State}
    end;

handle(~b"POST", _NoId, Req0, State) ->
    {ok, BodyBin, Req1} = cowboy_req:read_body(Req0),
    case decode_json(BodyBin) of
        {ok, #{~b"title" := Title}} when is_binary(Title), Title =/= <<>> ->
            {ok, Todo = #{~b"id" := NewId}} = todo_store:create(Title),
            Location =
                iolist_to_binary([~b"/todos/", integer_to_binary(NewId)]),
            Req =
                todo_http:json_reply(
                    Req1,
                    201,
                    Todo,
                    #{~b"location" => Location}
                ),
            {ok, Req, State};

        {ok, _} ->
            Req = todo_http:json_error(Req1, 400, ~b"missing_or_invalid_title"),
            {ok, Req, State};

        {error, _} ->
            Req = todo_http:json_error(Req1, 400, ~b"invalid_json"),
            {ok, Req, State}
    end;

handle(_Other, _Id, Req0, State) ->
    Req = todo_http:json_error(Req0, 405, ~b"method_not_allowed"),
    {ok, Req, State}.

parse_id(Req0) ->
    case cowboy_req:binding(id, Req0) of
        undefined ->
            {no_id, Req0};
        IdBin when is_binary(IdBin) ->
            case binary_to_integer(IdBin) of
                Id when is_integer(Id), Id > 0 ->
                    {ok, Id, Req0};
                _ ->
                    Req = todo_http:json_error(Req0, 400, ~b"invalid_id"),
                    {error, Req}
            end
    end.

decode_json(Bin) when is_binary(Bin) ->
    try
        {ok, jsx:decode(Bin, [return_maps])}
    catch
        _:_ ->
            {error, invalid_json}
    end.
