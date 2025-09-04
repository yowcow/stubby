-module(stubby_blackhole_handler).

-export([init/2]).


%% @doc A blackhole handler mounted to stubby server at <code>/blackhole/[...]</code>.
%% When requested, a response with status code 204 is returned.
-spec init(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
init(Req, State) ->
    Resp = cowboy_req:reply(204, #{}, <<>>, Req),
    {ok, Resp, State}.
