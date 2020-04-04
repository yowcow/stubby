-module(stubby).

-export([
         start/0,
         start/1,
         start/2,
         stop/0,
         get_recent/0
        ]).

%% See https://github.com/ninenines/cowboy/blob/master/src/cowboy_router.erl for details.
-type cowboy_route_match() :: '_' | iodata().
-type cowboy_route_path() :: {Path::cowboy_route_match(), Handler::module(), Opts::any()}.
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
    ok = stubby_recorder:start(),
    ok = application:start(ranch),
    Dispatch = cowboy_router:compile(
                 [
                  {'_',
                   Routes ++ [
                              {
                               "/",
                               stubby_default_handler,
                               #{}
                              },
                              {
                               "/blackhole/[...]",
                               stubby_blackhole_handler,
                               #{}
                              }
                             ]
                  }
                 ]
                ),
    {ok, _} = cowboy:start_clear(
                ?LISTENER,
                [{port, 0}],
                #{env => #{dispatch => Dispatch}}
               ),
    Url = lists:flatten(io_lib:format("http://~s:~p", [Host, ranch:get_port(?LISTENER)])),
    ok = check_ready(Url),
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
    ok = cowboy:stop_listener(?LISTENER),
    ok = application:stop(ranch),
    ok = stubby_recorder:stop(),
    error_logger:info_msg("server ~p is gone!", [?LISTENER]),
    ok.

%% @doc Fetches a record from the recorder FIFO queue.
%% If empty, the call is blocked until the next enqueue.
-spec get_recent() -> stubby_recorder:result().
get_recent() ->
    stubby_recorder:get_recent().
