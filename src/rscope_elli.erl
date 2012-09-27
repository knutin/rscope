-module(rscope_elli).
-behaviour(elli_handler).
-export([handle/2, handle_event/3]).

handle(Req, Config) ->
    rscope:identity((identity_fun(Config))(Req)),
    ignore.

handle_event(request_complete, _, _) ->
    rscope:request_complete(),
    ok;
handle_event(_, _, _) ->
    ok.


identity_fun(Config) ->
    proplists:get_value(identity_fun, Config, fun (_) -> <<"undefined">> end).
