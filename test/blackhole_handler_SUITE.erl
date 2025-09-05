-module(blackhole_handler_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


init_per_suite(Config) ->
    Path = "/blackhole/",
    Url = stubby:start() ++ Path,
    [{url, Url}, {path, Path} | Config].


end_per_suite(_Config) ->
    stubby:stop().


all() ->
    [request_plain_test, request_gzip_test, request_large_body_test].


request_plain_test(Config) ->
    Url = ?config(url, Config),
    Body = <<"Hello in plain text">>,
    {ok, {{"HTTP/1.1", 204, _}, _, _}} =
        httpc:request(post, {Url, [], "text/xml", Body}, [], []),
    {ok, [#{headers := Headers, body := Data} | _]} =
        stubby:get_recent("POST", ?config(path, Config)),
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
    {ok, [#{headers := Headers, body := Data} | _]} =
        stubby:get_recent("POST", ?config(path, Config)),
    [?assertMatch(#{
                    <<"content-type">> := <<"text/plain">>,
                    <<"content-encoding">> := <<"gzip">>
                   },
                  Headers),
     ?assertEqual(Body, Data)].


request_large_body_test(Config) ->
    Url = ?config(url, Config),
    % Create a body larger than default cowboy chunk size (8KB)
    LargeBody = binary:copy(<<"This is a test body that will be repeated many times to exceed the default chunk size. ">>, 200),
    {ok, {{"HTTP/1.1", 204, _}, _, _}} =
        httpc:request(post, {Url, [], "application/json", LargeBody}, [], []),
    {ok, [#{headers := Headers, body := Data} | _]} =
        stubby:get_recent("POST", ?config(path, Config)),
    [?assertMatch(#{<<"content-type">> := <<"application/json">>}, Headers),
     ?assertEqual(LargeBody, Data),
     ?assert(byte_size(LargeBody) > 8192)].
