-module(default_handler_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

% for cowboy handler
init(Req, State) ->
    Name = cowboy_req:binding(name, Req, <<"undef">>),
    Resp = cowboy_req:reply(200, #{}, Name, Req),
    {ok, Resp, State}.

init_per_suite(Config) ->
    Url = stubby:start([
                        {"/hello/[:name]", ?MODULE, []}
                       ]),
    [{url, Url} | Config].

end_per_suite(_) ->
    ok = stubby:stop(),
    ok.

all() ->
    [
     request_root_test,
     request_injected_route_test
    ].

request_root_test(Config) ->
    Url = ?config(url, Config),
    Result = httpc:request(Url ++ "/"),
    ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"}, _, "Hi"}}, Result).

request_injected_route_test(Config) ->
    Url = ?config(url, Config),
    Result = httpc:request(Url ++ "/hello/world"),
    ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"}, _, "world"}}, Result).
