-module(stubby_default_handler).

-export([init/2]).

%% @doc A default handler mounted to stubby server at path <code>/</code>.
-spec init(cowboy_req:req(), any()) -> {ok, cowboy_req:req(), any()}.
init(Req, State) ->
    Resp = cowboy_req:reply(200, #{}, <<"Hi">>, Req),
    {ok, Resp, State}.
