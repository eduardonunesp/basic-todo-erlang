-module(todo_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", todo_health_handler, []},
            {"/todos", todo_todos_handler, []},
            {"/todos/:id", todo_todo_handler, []}
        ]}
    ]),

    StoreChild =
        #{id => todo_store,
          start => {todo_store, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [todo_store]
        },

    CowboyChild =
        #{id => http_listener,
          start => {cowboy, start_clear, [http, [{port, 8080}], #{env => #{dispatch => Dispatch}}]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [cowboy]},

    {ok, {{one_for_one, 5, 10}, [StoreChild, CowboyChild]}}.

