%% @doc: `rscope` uses `seq_trace` for global variables scoped within
%% the execution of a single request.
%%
%% At this point you might say: "Oh, the horror! What kind of twisted
%% use case could there be for this?" It's very easy, you can set a
%% variable early in the execution of the request (for example
%% "identity" can be set during the HTTP handling) and later used when
%% collecting metrics or logging.
%%
%% It can work something like this:
%%
%% 1. In your HTTP handler, call request:identity/1 to identify the
%%    request as "fetch document", "update invoice", etc.
%%
%% 2. In your business logic in another process, call
%%    rscope:identity/0 to retrieve the identity and use it for
%%    collecting metrics or logging.
%%
%% 3. When the request is complete, ie. you are back in the HTTP
%% handler returning the response, call rscope:request_complete/0 to
%% clean up the cached request identity.

-module(rscope).
-include_lib("eunit/include/eunit.hrl").
-export([init/0, request_complete/0, identity/1, identity/0]).

-define(ID_TABLE, rscope_identities).
-define(LABEL_TABLE, rscope_labels).

%% @doc: Creates the ETS-tables used to store request ephemeral
%% identities. The tables will be owned by the caller, so it should be
%% wrappen in a supervised gen_server.
init() ->
    ets:new(?ID_TABLE, [named_table, public, set]),
    ets:new(?LABEL_TABLE, [named_table, public, set]),
    ets:insert(?LABEL_TABLE, {current_label, 0}),
    ok.


-spec identity(term()) -> ok.
%% @doc: Identify the current request. If there is already an identity
%% set, a new identity is set and later calls to identity/0 will
%% return the new identity.
identity(Id) ->
    Label = ets:update_counter(?LABEL_TABLE, current_label, 1),
    seq_trace:set_token(label, Label),
    true = ets:insert_new(?ID_TABLE, {Label, Id}),
    ok.

-spec identity() -> term() | undefined.
%% @doc: Returns the identity as set by identity/1. If no identity is
%% set, returns the atom 'undefined.'
identity() ->
    case seq_trace:get_token(label) of
        {label, Label} ->
            case ets:lookup(?ID_TABLE, Label) of
                [{Label, Id}] ->
                    Id;
                [] ->
                    undefined
            end;
        [] ->
            undefined
    end.

%% @doc: Cleanes up the ephemeral identity of the request. As this is
%% the only way of pruning the identities table, it should be called
%% on every request end.
request_complete() ->
    case seq_trace:get_token(label) of
        {label, Label} ->
            ets:delete(?ID_TABLE, Label),
            seq_trace:set_token([]),
            ok;
        [] ->
            ok
    end.


%%
%% TESTS
%%

simple_test() ->
    init(),

    ?assertEqual([], seq_trace:get_token(label)),

    ?assertEqual(ok, identity(foobar)),
    ?assertEqual({label, 1}, seq_trace:get_token(label)),
    ?assertEqual(foobar, identity()),

    request_complete(),
    ?assertEqual([], seq_trace:get_token(label)),
    ?assertEqual([], ets:tab2list(?ID_TABLE)).

