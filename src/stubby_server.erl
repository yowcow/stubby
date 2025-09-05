-module(stubby_server).

-behavior(gen_server).

-export([init/1, handle_cast/2, handle_call/3]).
-export([start/0, stop/0, enqueue/2, dequeue/1, reset/0]).

-include("stubby.hrl").

-include_lib("kernel/include/logger.hrl").

-type method() :: binary().
-type path() :: binary().
-type val() :: stubby_record().
-type key() :: {method(), path()}.
-type state() :: #{key() => [val()]}.


-spec init(any()) -> {ok, state()}.
init(_) ->
    ?LOG_INFO("stubby_server started", []),
    {ok, maps:new()}.


-spec handle_cast({key(), val()}, state()) -> {noreply, state()}.
handle_cast({Key, Val}, State) ->
    NewState =
        case maps:find(Key, State) of
            {ok, Vs} ->
                maps:put(Key, [Val | Vs], State);
            _ ->
                maps:put(Key, [Val], State)
        end,
    ?LOG_INFO("stubby_server enqueued ~p => ~p", [Key, Val]),
    {noreply, NewState}.


-spec handle_call({dequeue, key()}, {pid(), gen_server:reply_tag()}, state()) ->
          {reply, {ok, [val()]}, state()};
                 (reset, {pid(), gen_server:reply_tag()}, state()) ->
          {reply, {ok, state()}, state()}.
handle_call({dequeue, Key}, _From, State) ->
    Vs = maps:get(Key, State, []),
    NewState = maps:remove(Key, State),
    ?LOG_INFO("stubby_server dequeued ~p => ~p", [Key, Vs]),
    {reply, {ok, Vs}, NewState};
handle_call(reset, _From, State) ->
    ?LOG_INFO("stubby_server reset from ~p to empty", [State]),
    {reply, {ok, State}, maps:new()}.


-spec start() -> {ok, pid()}.
start() ->
    ?LOG_INFO("stubby_server starting", []),
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).


-spec stop() -> ok.
stop() ->
    ?LOG_INFO("stubby_server stopping", []),
    gen_server:stop(?MODULE).


-spec enqueue(key(), val()) -> ok.
enqueue(Key, Val) ->
    gen_server:cast(?MODULE, {Key, Val}).


-spec dequeue(key()) -> {ok, [val()]}.
dequeue(Key) ->
    gen_server:call(?MODULE, {dequeue, Key}).


-spec reset() -> {ok, state()}.
reset() ->
    gen_server:call(?MODULE, reset).
