-module(blackhole_handler_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

init_per_suite(Config) ->
    Path = "/blackhole/",
    Url = stubby:start() ++ Path,
    [{url, Url}, {path, Path} | Config].

end_per_suite(_) ->
    ok = stubby:stop(),
    ok.

all() ->
    [request_plain_test, request_gzip_test, slow_request_awaits_test].

request_plain_test(Config) ->
    Url = ?config(url, Config),
    Body = <<"Hello in plain text">>,
    {ok, {{"HTTP/1.1", 204, _}, _, _}} =
        httpc:request(post, {Url, [], "text/xml", Body}, [], []),
    {ok, #{headers := Headers, body := Data}} = stubby:get_recent(?config(path, Config)),
    [?assertMatch(#{<<"content-type">> := <<"text/xml">>}, Headers),
     ?assertEqual(Body, Data)].

request_gzip_test(Config) ->
    Url = ?config(url, Config),
    Body = <<"Hello in gzipped text">>,
    {ok, {{"HTTP/1.1", 204, _}, _, _}} =
        httpc:request(post,
                      {Url, [{"content-encoding", "gzip"}], "text/plain", zlib:gzip(Body)},
                      [],
                      []),
    {ok, #{headers := Headers, body := Data}} = stubby:get_recent(?config(path, Config)),
    [?assertMatch(#{<<"content-type">> := <<"text/plain">>,
                    <<"content-encoding">> := <<"gzip">>},
                  Headers),
     ?assertEqual(Body, Data)].

slow_request_awaits_test(Config) ->
    Url = ?config(url, Config),
    Reqs = [{<<"slow req after 100 ms">>, 100}, {<<"fast req after 10 ms">>, 10}],
    [spawn(fun() ->
              timer:sleep(Time),
              httpc:request(post, {Url, [], "text/plain", Body}, [], [])
           end)
     || {Body, Time} <- Reqs],
    ?assertEqual([<<"fast req after 10 ms">>, <<"slow req after 100 ms">>],
                 [Data
                  || {ok, #{body := Data}}
                         <- [stubby:get_recent(?config(path, Config)),
                             stubby:get_recent(?config(path, Config))]]).
