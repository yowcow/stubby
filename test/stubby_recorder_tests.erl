-module(stubby_recorder_tests).

-include_lib("eunit/include/eunit.hrl").

enqueue_test_() ->
    Cases = [
             {
              "0 element",
              foo,
              [],
              [foo]
             },
             {
              "1 element",
              bar,
              [foo],
              [bar, foo]
             },
             {
              "2 elements",
              buz,
              [bar, foo],
              [buz, bar, foo]
             }
            ],
    F = fun({Title, Input, Queue, Expected}) ->
                Actual = stubby_recorder:enqueue(Input, Queue),
                {Title, ?_assertEqual(Expected, Actual)}
        end,
    lists:map(F, Cases).

dequeue_test_() ->
    Cases = [
             {
              "1 element",
              [foo],
              {foo, []}
             },
             {
              "2 elements",
              [bar, foo],
              {foo, [bar]}
             },
             {
              "3 elements",
              [buz, bar, foo],
              {foo, [buz, bar]}
             },
             {
              "4 elements",
              [hoge, buz, bar, foo],
              {foo, [hoge, buz, bar]}
             }
            ],
    F = fun({Title, Input, Expected}) ->
                Actual = stubby_recorder:dequeue(Input),
                {Title, ?_assertEqual(Expected, Actual)}
        end,
    lists:map(F, Cases).

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
                       Input = [foo, bar, buz],
                       [stubby_recorder:put_recent(In) || In <- Input],
                       Expected = [
                                   {ok, foo},
                                   {ok, bar},
                                   {ok, buz}
                                  ],
                       ?assertEqual(Expected,
                                    [
                                     stubby_recorder:get_recent(),
                                     stubby_recorder:get_recent(),
                                     stubby_recorder:get_recent()
                                    ]
                                   )
               end
              },
              {
               "put/get in async",
               fun() ->
                       Input = [{buz, 100},
                                {bar, 50},
                                {foo, 10}
                               ],
                       [spawn(fun() ->
                                     timer:sleep(Time),
                                     stubby_recorder:put_recent(In)
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
                                     stubby_recorder:get_recent(),
                                     stubby_recorder:get_recent(),
                                     stubby_recorder:get_recent()
                                    ]
                                   )
               end
              }
             ]
     end
    }.
