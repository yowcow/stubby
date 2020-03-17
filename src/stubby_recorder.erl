-module(stubby_recorder).

-export([
         start/0,
         stop/0,
         put_recent/1,
         get_recent/0
        ]).
-export_type([
              body/0,
              result/0
             ]).

-type body() :: binary().
-type result() :: {ok, body()} | {error, no_data}.

-define(RECORDER, ?MODULE).

-spec start() -> ok.
start() ->
    true = register(?RECORDER, spawn(fun start_recorder/0)),
    ok.

-spec stop() -> ok.
stop() ->
    stop_recorder().

start_recorder() ->
    loop([]).

loop(Stack) ->
    receive
        {put, From, Data} ->
            From ! ok,
            loop([Data|Stack]);
        {get, From} ->
            case Stack of
                [Data|L] ->
                    From ! {ok, Data},
                    loop(L);
                [] ->
                    From ! {error, no_data},
                    loop([])
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
