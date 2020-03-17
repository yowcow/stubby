-module(stubby_recorder_tests).

-include_lib("eunit/include/eunit.hrl").

put_get_test_() ->
    {setup,
     fun() ->
             ok = stubby_recorder:start(),
             ok
     end,
     fun(ok) ->
             ok = stubby_recorder:stop()
     end,
     fun(ok) ->
             [
              {"put foo", ?_assertEqual(ok, stubby_recorder:put_recent(foo))},
              {"put bar", ?_assertEqual(ok, stubby_recorder:put_recent(bar))},
              {"put buz", ?_assertEqual(ok, stubby_recorder:put_recent(buz))},
              {"get buz", ?_assertEqual({ok, buz}, stubby_recorder:get_recent())},
              {"get bar", ?_assertEqual({ok, bar}, stubby_recorder:get_recent())},
              {"get foo", ?_assertEqual({ok, foo}, stubby_recorder:get_recent())},
              {"no more", ?_assertEqual({error, no_data}, stubby_recorder:get_recent())}
             ]
     end
    }.
