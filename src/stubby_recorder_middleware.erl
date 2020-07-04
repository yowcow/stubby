-module(stubby_recorder_middleware).

-export([execute/2]).

-type stubby_record() :: #{
                           headers := map(),
                           scheme := binary(),
                           host := binary(),
                           port := integer(),
                           path := binary(),
                           qs := binary(),
                           body := binary()
                          }.

%% @doc A request recording middleware for all routes.
%% When requested, the request is enqueued to recorder FIFO queue.
%% A request body can be gzipped when <code>Content-Encoding: gzip</code> is specified.
execute(#{path := Path} = Req0, Env) ->
    {ok, Data0, Req} = cowboy_req:read_body(Req0, #{length => infinity}),
    Encoding = cowboy_req:header(<<"content-encoding">>, Req0, undefined),
    Data = decode_body(Data0, Encoding),
    ok = stubby_recorder:put_recent(Path, build_record(Req0#{body => Data})),
    {ok, Req, Env}.

%% @private
decode_body(Data, <<"gzip">>) ->
    zlib:gunzip(Data);
decode_body(Data, undefined) ->
    Data;
decode_body(_, Encoding) ->
    throw({content_encoding, Encoding}).

%% @private
-spec build_record(Req::term()) -> stubby_record().
build_record(Req) ->
    build_record(
      [headers, scheme, host, port, path, qs, body],
      Req,
      #{}
     ).

%% @private
build_record([], _, Acc) ->
    Acc;
build_record([K|T],  Req, Acc) ->
    build_record(T, Req, Acc#{K => maps:get(K, Req)}).
