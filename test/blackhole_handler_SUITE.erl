-module(blackhole_handler_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

init_per_suite(Config) ->
    Url = stubby:start() ++ "/blackhole/",
    [{url, Url} | Config].

end_per_suite(_) ->
    ok = stubby:stop(),
    ok.

all() ->
    [
     request_plain_test,
     request_gzip_test,
     slow_request_with_retry_ok_test,
     never_succeed_retry_test
    ].

request_plain_test(Config) ->
    Url = ?config(url, Config),
    Body = <<"Hello in plain text">>,
    {ok, {
       {"HTTP/1.1", 204, _},
       _,
       _
      }} = httpc:request(
             post,
             {Url, [], "text/plain", Body},
             [],
             []
            ),
    ?assertEqual({ok, Body}, stubby:get_recent()).

request_gzip_test(Config) ->
    Url = ?config(url, Config),
    Body = <<"Hello in gzipped text">>,
    {ok, {
       {"HTTP/1.1", 204, _},
       _,
       _
      }} = httpc:request(
             post,
             {Url, [{"content-encoding", "gzip"}], "text/plain", zlib:gzip(Body)},
             [],
             []
            ),
    ?assertEqual({ok, Body}, stubby:get_recent()).

slow_request_with_retry_ok_test(Config) ->
    Url = ?config(url, Config),
    Body = <<"sent after 50 ms">>,
    spawn(fun() ->
                  timer:sleep(50), % make a request with delay
                  httpc:request(
                    post,
                    {Url, [], "text/plain", Body},
                    [],
                    []
                   )
          end),
    Result = stubby:get_recent(
               [
                {retry, 1},
                {interval, 100}
               ]
              ), % retry with interval after the first failure
    ?assertEqual({ok, Body}, Result).

never_succeed_retry_test(_) ->
    Result = stubby:get_recent(
               [
                {retry, 2},
                {interval, 50}
               ]
              ), % retry with interval after the first failure
    ?assertEqual({error, no_data}, Result).
