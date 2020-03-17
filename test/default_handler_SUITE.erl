-module(default_handler_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

init_per_suite(Config) ->
    Url = stubby:start() ++ "/",
    [{url, Url} | Config].

end_per_suite(_) ->
    ok = stubby:stop(),
    ok.

all() ->
    [
     request_test
    ].

request_test(Config) ->
    Url = ?config(url, Config),
    Result = httpc:request(Url),
    ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"}, _, "Hi"}}, Result).
