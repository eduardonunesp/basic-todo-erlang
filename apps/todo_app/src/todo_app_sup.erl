-module(todo_app_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Dispatch = cowboy_router:compile([{'_', [{"/health", todo_health_handler, []}]}]),

    CowboyChild =
        #{id => http_listener,
          start => {cowboy, start_clear, [http, [{port, 8080}], #{env => #{dispatch => Dispatch}}]},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [cowboy]},

    {ok, {{one_for_one, 5, 10}, [CowboyChild]}}.

%% internal functions
