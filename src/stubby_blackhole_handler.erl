-module(stubby_blackhole_handler).

-export([init/2]).

%% @doc A request recording handler mounted to stubby server at <code>/blackhole/[...]</code>.
%% When requested, the request body is enqueued to recorder FIFO queue, and a response with status code 204 is returned.
%% A request body can be gzipped when <code>Content-Encoding: gzip</code> is specified.
-spec init(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
init(Req0, State) ->
    {ok, Data0, Req} = cowboy_req:read_body(Req0, #{length => infinity}),
    Encoding = cowboy_req:header(<<"content-encoding">>, Req0, undefined),
    Data = decode_body(Data0, Encoding),
    ok = stubby_recorder:put_recent(Data),
    Resp = cowboy_req:reply(204, #{}, <<>>, Req),
    {ok, Resp, State}.

decode_body(Data, <<"gzip">>) ->
    zlib:gunzip(Data);
decode_body(Data, undefined) ->
    Data;
decode_body(_, Encoding) ->
    throw({content_encoding, Encoding}).
