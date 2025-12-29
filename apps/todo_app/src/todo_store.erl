-module(todo_store).
-behaviour(gen_server).

-export([start_link/0]).
-export([list/0, create/1, get/1, update/2, delete/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TABLE, todo_items).

-record(state, {next_id = 1 :: pos_integer()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Public API
list() ->
    gen_server:call(?MODULE, list).

create(TitleBin) when is_binary(TitleBin) ->
    gen_server:call(?MODULE, {create, TitleBin}).

get(Id) ->
    gen_server:call(?MODULE, {get, Id}).

update(Id, PatchMap) when is_map(PatchMap) ->
    gen_server:call(?MODULE, {update, Id, PatchMap}).

delete(Id) ->
    gen_server:call(?MODULE, {delete, Id}).

%% gen_server callbacks
init([]) ->
    _ = ets:new(?TABLE, [named_table, set, public]),
    {ok, #state{}}.

handle_call(list, _From, State) ->
    Todos = [V || {_K, V} <- ets:tab2list(?TABLE)],
    {reply, lists:sort(fun by_id/2, Todos), State};

handle_call({create, Title}, _From, #state{next_id = Id} = State) ->
    Todo = #{
        ~b"id" => Id,
        ~b"title" => Title,
        ~b"done" => false
    },
    true = ets:insert(?TABLE, {Id, Todo}),
    {reply, {ok, Todo}, State#state{next_id = Id + 1}};

handle_call({get, Id}, _From, State) ->
    Reply =
        case ets:lookup(?TABLE, Id) of
            [{_Id, Todo}] -> {ok, Todo};
            [] -> not_found
        end,
    {reply, Reply, State};

handle_call({update, Id, Patch}, _From, State) ->
    Reply =
        case ets:lookup(?TABLE, Id) of
            [{_Id, Todo0}] ->
                Todo1 = apply_patch(Todo0, Patch),
                true = ets:insert(?TABLE, {Id, Todo1}),
                {ok, Todo1};
            [] ->
                not_found
        end,
    {reply, Reply, State};

handle_call({delete, Id}, _From, State) ->
    Reply =
        case ets:lookup(?TABLE, Id) of
            [{_Id, _Todo}] ->
                true = ets:delete(?TABLE, Id),
                ok;
            [] ->
                not_found
        end,
    {reply, Reply, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.

by_id(#{~b"id" := A}, #{~b"id" := B}) -> A =< B.

apply_patch(Todo0, Patch) ->
    Todo1 =
        case maps:find(~b"title", Patch) of
            {ok, T} when is_binary(T) -> Todo0#{~b"title" => T};
            _ -> Todo0
        end,
    case maps:find(~b"done", Patch) of
        {ok, D} when is_boolean(D) -> Todo1#{~b"done" => D};
        _ -> Todo1
    end.
