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
    Url = stubby:start([{"/hello/[:name]", ?MODULE, []}]),
    [{url, Url} | Config].

end_per_suite(_) ->
    stubby:stop().

all() ->
    [request_root_test,
     request_injected_route_test,
     recorded_injected_route_test,
     recorded_root_test].

request_root_test(Config) ->
    Url = ?config(url, Config),
    Result = httpc:request(Url ++ "/"),
    ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"}, _, "Hi"}}, Result).

request_injected_route_test(Config) ->
    Url = ?config(url, Config),
    Result = httpc:request(get, {Url ++ "/hello/world?foo=bar", []}, [], []),
    ?assertMatch({ok, {{"HTTP/1.1", 200, "OK"}, _, "world"}}, Result).

recorded_root_test(_) ->
    {ok, [Record]} = stubby:get_recent("GET", "/"),
    ?assertMatch(#{body := <<>>,
                   path := <<"/">>,
                   qs := <<>>},
                 Record).

recorded_injected_route_test(_) ->
    {ok, [Record]} = stubby:get_recent("GET", "/hello/world"),
    ?assertMatch(#{body := <<>>,
                   path := <<"/hello/world">>,
                   qs := <<"foo=bar">>},
                 Record).
