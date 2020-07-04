-module(stubby_recorder).

-export([
         start/0,
         stop/0,
         put_recent/2,
         get_recent/1
        ]).

-export_type([
              body/0,
              result/0
             ]).

-type body() :: term().
-type result() :: {ok, body()}.

-define(RECORDER, ?MODULE).

%% @doc Starts and registers a recorder process.
-spec start() -> ok.
start() ->
    true = register(?RECORDER, spawn(fun start_recorder/0)),
    ok.

%% @doc Stops running recorder process.
-spec stop() -> ok.
stop() ->
    stop_recorder().

%% @private
start_recorder() ->
    loop(#{}, #{}).

%% @private
enqueue(Key, Item, Tree) ->
    case Tree of
        #{Key := Queue} ->
            Tree#{Key => [Item|Queue]};
        _ ->
            Tree#{Key => [Item]}
    end.

%% @private
dequeue(Key, Tree) ->
    case Tree of
        #{Key := []} ->
            none;
        #{Key := Queue} ->
            [Item|T] = lists:reverse(Queue),
            {Item, Tree#{Key => lists:reverse(T)}};
        _ ->
            none
    end.

%% @private
loop(Records, Getters) ->
    receive
        {put, From, Key, Data} ->
            From ! ok,
            case dequeue(Key, Getters) of
                {KeyGetter, Getters1} ->
                    KeyGetter ! {ok, Data},
                    loop(Records, Getters1);
                _ ->
                    loop(enqueue(Key, Data, Records), Getters)
            end;
        {get, Getter, Key} ->
            case dequeue(Key, Records) of
                {Data, Records1} ->
                    Getter ! {ok, Data},
                    loop(Records1, Getters);
                _ ->
                    loop(Records, enqueue(Key, Getter, Getters))
            end;
        {quit, From} ->
            From ! ok,
            ok
    end.

%% @doc Puts a record into FIFO queue.
-spec put_recent(term(), body()) -> ok.
put_recent(Key, Data) ->
    ?RECORDER ! {put, self(), Key, Data},
    receive X -> X end.

%% @doc Gets a record from FIFO queue.
%% If empty, the call is blocked until the next enqueue.
-spec get_recent(term()) -> result().
get_recent(Key) ->
    ?RECORDER ! {get, self(), Key},
    receive X -> X end.

stop_recorder() ->
    ?RECORDER ! {quit, self()},
    receive X -> X end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

enqueue_test_() ->
    Cases = [
             {
              "0 element",
              foo,
              foo,
              #{bar => []},
              #{
                foo => [foo],
                bar => []
               }
             },
             {
              "1 element",
              foo,
              bar,
              #{
                foo => [foo],
                bar => []
               },
              #{
                foo => [bar, foo],
                bar => []
               }
             },
             {
              "2 elements",
              foo,
              buz,
              #{
                foo => [bar, foo],
                bar => []
               },
              #{
                foo => [buz, bar, foo],
                bar => []
               }
             }
            ],
    F = fun({Title, Key, Input, Queue, Expected}) ->
                Actual = enqueue(Key, Input, Queue),
                {Title, ?_assertEqual(Expected, Actual)}
        end,
    lists:map(F, Cases).

dequeue_test_() ->
    Cases = [
             {
              "0 element",
              bar,
              #{
                foo => [foo],
                bar => []
               },
              none
             },
             {
              "1 element",
              foo,
              #{
                foo => [foo],
                bar => []
               },
              {foo, #{
                      foo => [],
                      bar => []
                     }}
             },
             {
              "2 elements",
              foo,
              #{
                foo => [bar, foo],
                bar => []
               },
              {foo, #{
                      foo => [bar],
                      bar => []
                     }}
             },
             {
              "3 elements",
              foo,
              #{
                foo => [buz, bar, foo],
                bar => []
               },
              {foo, #{
                      foo => [buz, bar],
                      bar => []
                     }}
             }
            ],
    F = fun({Title, Key, Input, Expected}) ->
                Actual = dequeue(Key, Input),
                {Title, ?_assertEqual(Expected, Actual)}
        end,
    lists:map(F, Cases).
-endif.
