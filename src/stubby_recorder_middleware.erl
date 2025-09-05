-module(stubby_recorder_middleware).

-export([execute/2]).

-include("stubby.hrl").


%% @doc A request recording middleware for all routes.
%% When requested, the request is enqueued to recorder FIFO queue.
%% A request body can be gzipped when <code>Content-Encoding: gzip</code> is specified.
execute(Req0, Env) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    {ok, Data0, Req} = cowboy_req:read_body(Req0),
    Encoding = cowboy_req:header(<<"content-encoding">>, Req0, undefined),
    Data = decode_body(Data0, Encoding),
    ok = stubby_server:enqueue({Method, Path}, build_record(Req0#{body => Data})),
    {ok, Req, Env}.


%% @private
decode_body(Data, <<"gzip">>) ->
    zlib:gunzip(Data);
decode_body(Data, undefined) ->
    Data;
decode_body(_, Encoding) ->
    throw({content_encoding, Encoding}).


%% @private
-spec build_record(Req :: term()) -> stubby_record().
build_record(Req) ->
    build_record([headers, scheme, host, port, path, qs, body], Req, #{}).


%% @private
build_record([], _, Acc) ->
    Acc;
build_record([K | T], Req, Acc) ->
    build_record(T, Req, Acc#{K => maps:get(K, Req)}).
