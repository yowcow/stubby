-module(stubby).

-export([start/0, start/1, start/2, stop/0, get_recent/2, reset/0]).

-include("stubby.hrl").

%% See https://github.com/ninenines/cowboy/blob/master/src/cowboy_router.erl for details.
-type cowboy_route_match() :: '_' | iodata().
-type cowboy_route_path() ::
        {Path :: cowboy_route_match(), Handler :: module(), Opts :: any()}.
-type url() :: string().

-define(LISTENER, ?MODULE).


%% @doc Starts a stubby server with default configuration.
-spec start() -> url().
start() ->
    start([]).


%% @doc Starts a stubby server with optional routes.
-spec start([cowboy_route_path()]) -> url().
start(Routes) ->
    start("localhost", Routes).


%% @doc Starts a stubby server with optional hostname and routes configurations.
-spec start(string(), [cowboy_route_path()]) -> url().
start(Host, Routes) ->
    ok = application:start(ranch),
    Dispatch =
        cowboy_router:compile([{'_',
                                Routes ++
                                [{"/", stubby_default_handler, #{}},
                                 {"/blackhole/[...]", stubby_blackhole_handler, #{}}]}]),
    {ok, _} =
        cowboy:start_clear(?LISTENER,
                           [{port, 0}],
                           #{
                             env => #{dispatch => Dispatch},
                             middlewares =>
                                 [cowboy_router, stubby_recorder_middleware, cowboy_handler]
                            }),
    Url = lists:flatten(
            io_lib:format("http://~s:~p", [Host, ranch:get_port(?LISTENER)])),
    ok = check_ready(Url),
    {ok, _} = stubby_server:start(),
    Url.


%% @private
check_ready(Url) ->
    case httpc:request(Url ++ "/") of
        {ok, _} ->
            error_logger:info_msg("server ~p is ready!", [?LISTENER]),
            ok;
        Err ->
            error_logger:info_msg("server ~p is not ready? :: ~p", [?LISTENER, Err]),
            check_ready(Url)
    end.


%% @doc Stops a running stubby server.
-spec stop() -> ok.
stop() ->
    ok = stubby_server:stop(),
    ok = cowboy:stop_listener(?LISTENER),
    ok = application:stop(ranch),
    error_logger:info_msg("server ~p is gone!", [?LISTENER]),
    ok.


%% @doc Fetches a record from the recorder FIFO queue.
%% If empty, the call is blocked until the next enqueue.
-spec get_recent(Method :: string(), Path :: string()) -> {ok, [stubby_record()]}.
get_recent(Method, Path) ->
    stubby_server:dequeue({list_to_binary(Method), list_to_binary(Path)}).


-spec reset() -> {ok, term()}.
reset() ->
    stubby_server:reset().
