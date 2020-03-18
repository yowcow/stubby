-module(stubby).

-export([
         start/0,
         start/1,
         start/2,
         stop/0,
         get_recent/0,
         get_recent/1
        ]).

-type cowboy_route_match() :: '_' | iodata().
-type cowboy_route_path() :: {Path::cowboy_route_match(), Handler::module(), Opts::any()}.
-type url() :: string().

-type get_recent_option() :: {retry, integer()} | {interval, integer()}.
-type get_recent_options() :: [get_recent_option()].

-define(LISTENER, ?MODULE).

-spec start() -> url().
start() ->
    start([]).

-spec start([cowboy_route_path()]) -> url().
start(Routes) ->
    start("localhost", Routes).

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

check_ready(Url) ->
    case httpc:request(Url ++ "/") of
        {ok, _} ->
            error_logger:info_msg("server ~p is ready!", [?LISTENER]),
            ok;
        Err ->
            error_logger:info_msg("server ~p is not ready? :: ~p", [?LISTENER, Err]),
            check_ready(Url)
    end.

-spec stop() -> ok.
stop() ->
    ok = cowboy:stop_listener(?LISTENER),
    ok = application:stop(ranch),
    ok = stubby_recorder:stop(),
    error_logger:info_msg("server ~p is gone!", [?LISTENER]),
    ok.

-spec get_recent() -> stubby_recorder:result().
get_recent() ->
    stubby_recorder:get_recent().

-spec get_recent(get_recent_options()) -> stubby_recorder:result().
get_recent(Options) ->
    Retry = proplists:get_value(retry, Options, 0),
    Interval = proplists:get_value(interval, Options, 0),
    get_recent(Retry, Interval).

get_recent(0, _) ->
    stubby_recorder:get_recent();
get_recent(Retry, Interval) ->
    case stubby_recorder:get_recent() of
        {ok, _} = Result ->
            Result;
        _ ->
            timer:sleep(Interval),
            get_recent(Retry - 1, Interval)
    end.
