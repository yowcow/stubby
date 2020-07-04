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
              {"put/get in fifo order",
               fun() ->
                       Key = key,
                       Input = [foo, bar, buz],
                       [stubby_recorder:put_recent(Key, In) || In <- Input],
                       Expected = [
                                   {ok, foo},
                                   {ok, bar},
                                   {ok, buz}
                                  ],
                       ?assertEqual(Expected,
                                    [
                                     stubby_recorder:get_recent(Key),
                                     stubby_recorder:get_recent(Key),
                                     stubby_recorder:get_recent(Key)
                                    ]
                                   )
               end
              },
              {
               "put/get in async",
               fun() ->
                       Key = key,
                       Input = [{buz, 100},
                                {bar, 50},
                                {foo, 10}
                               ],
                       [spawn(fun() ->
                                     timer:sleep(Time),
                                     stubby_recorder:put_recent(Key, In)
                              end)
                        || {In, Time} <- Input
                       ],
                       Expected = [
                                   {ok, foo},
                                   {ok, bar},
                                   {ok, buz}
                                  ],
                       ?assertEqual(Expected,
                                    [
                                     stubby_recorder:get_recent(Key),
                                     stubby_recorder:get_recent(Key),
                                     stubby_recorder:get_recent(Key)
                                    ]
                                   )
               end
              }
             ]
     end
    }.
