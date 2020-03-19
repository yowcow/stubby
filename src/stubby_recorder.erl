-module(stubby_recorder).

-export([
         start/0,
         stop/0,
         put_recent/1,
         get_recent/0
        ]).

-ifdef(TEST).
-export([
         enqueue/2,
         dequeue/1
        ]).
-endif.

-export_type([
              body/0,
              result/0
             ]).

-type body() :: binary().
-type result() :: {ok, body()}.

-define(RECORDER, ?MODULE).

-spec start() -> ok.
start() ->
    true = register(?RECORDER, spawn(fun start_recorder/0)),
    ok.

-spec stop() -> ok.
stop() ->
    stop_recorder().

start_recorder() ->
    loop([], []).

enqueue(Item, Queue) ->
    [Item | Queue].

dequeue(Queue) ->
    [Item|L] = lists:reverse(Queue),
    {Item, lists:reverse(L)}.

loop(Records, Getters) ->
    receive
        {put, From, Data} ->
            From ! ok,
            case Getters of
                [] ->
                    loop(enqueue(Data, Records), []);
                _ ->
                    {Getter, L} = dequeue(Getters),
                    Getter ! {ok, Data},
                    loop(Records, L)
            end;
        {get, Getter} ->
            case Records of
                [] ->
                    loop([], enqueue(Getter, Getters));
                _ ->
                    {Data, L} = dequeue(Records),
                    Getter! {ok, Data},
                    loop(L, Getters)
            end;
        {quit, From} ->
            From ! ok,
            ok
    end.

-spec put_recent(body()) -> ok.
put_recent(Data) ->
    ?RECORDER ! {put, self(), Data},
    receive X -> X end.

-spec get_recent() -> result().
get_recent() ->
    ?RECORDER ! {get, self()},
    receive X -> X end.

stop_recorder() ->
    ?RECORDER ! {quit, self()},
    receive X -> X end.
