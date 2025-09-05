-module(stubby_server_tests).

-include_lib("eunit/include/eunit.hrl").

server_test_() ->
    {setup,
     fun() -> stubby_server:start() end,
     fun(_) -> stubby_server:stop() end,
     fun(_) ->
        ok = stubby_server:enqueue({get, "/"}, req1),
        ok = stubby_server:enqueue({get, "/"}, req2),
        {ok, Ret1} = stubby_server:dequeue({get, "/"}),
        ok = stubby_server:enqueue({get, "/1"}, req3),
        ok = stubby_server:enqueue({get, "/2"}, req4),
        {ok, Ret2} = stubby_server:reset(),
        {ok, Ret3} = stubby_server:reset(),
        [?_assertEqual([req2, req1], Ret1),
         ?_assertEqual(#{{get, "/1"} => [req3], {get, "/2"} => [req4]}, Ret2),
         ?_assertEqual(#{}, Ret3)]
     end}.
