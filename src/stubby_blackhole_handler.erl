-module(stubby_blackhole_handler).

-export([init/2]).

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
