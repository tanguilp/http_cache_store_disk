-module(distribution_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([broadcast/1, warmup/1, invalidate_url/1, invalidate_by_alternate_key/1]).

-define(TEST_REQUEST_KEY, <<"some request key">>).
-define(TEST_URL_DIGEST, <<"request URI digest">>).
-define(TEST_VARY_HEADERS, #{}).
-define(TEST_STATUS, 200).
-define(TEST_RESP_HEADERS, [{<<"content-length">>, <<"21">>}]).
-define(TEST_RESP_BODY, <<"Some response content">>).
-define(TEST_RESPONSE, {?TEST_STATUS, ?TEST_RESP_HEADERS, ?TEST_RESP_BODY}).
-define(TEST_RESP_METADATA,
        #{alternate_keys => [some, alternate, keys], grace => erlang:system_time(second) + 120}).
-define(TEST_OPTS, []).

-record(peer, {peer, node, cache_dir}).

all() ->
    [broadcast, warmup, invalidate_url, invalidate_by_alternate_key].

init_per_testcase(TestName, Config) ->
    Peers = [configure_peer(PeerIndex, Config, TestName) || PeerIndex <- lists:seq(1, 2)],
    [{peers, Peers} | Config].

end_per_testcase(_, Config) ->
    [file:del_dir_r(Peer#peer.cache_dir) || Peer <- ?config(peers, Config)].

broadcast(Config) ->
    [PeerA, PeerB] = ?config(peers, Config),
    connect_peers(PeerA, PeerB),
    peer:call(PeerA#peer.peer,
              http_cache_store_disk,
              put,
              [?TEST_REQUEST_KEY,
               ?TEST_URL_DIGEST,
               ?TEST_VARY_HEADERS,
               ?TEST_RESPONSE,
               ?TEST_RESP_METADATA,
               ?TEST_OPTS]),
    timer:sleep(100),
    [_] =
        peer:call(PeerA#peer.peer,
                  http_cache_store_disk,
                  list_candidates,
                  [?TEST_REQUEST_KEY, ?TEST_OPTS]),
    [_] =
        peer:call(PeerB#peer.peer,
                  http_cache_store_disk,
                  list_candidates,
                  [?TEST_REQUEST_KEY, ?TEST_OPTS]).

warmup(Config) ->
    [PeerA, PeerB] = ?config(peers, Config),
    peer:call(PeerA#peer.peer,
              http_cache_store_disk,
              put,
              [?TEST_REQUEST_KEY,
               ?TEST_URL_DIGEST,
               ?TEST_VARY_HEADERS,
               ?TEST_RESPONSE,
               ?TEST_RESP_METADATA,
               ?TEST_OPTS]),
    timer:sleep(100),
    [_] =
        peer:call(PeerA#peer.peer,
                  http_cache_store_disk,
                  list_candidates,
                  [?TEST_REQUEST_KEY, ?TEST_OPTS]),
    connect_peers(PeerA, PeerB),
    timer:sleep(100),
    [_] =
        peer:call(PeerB#peer.peer,
                  http_cache_store_disk,
                  list_candidates,
                  [?TEST_REQUEST_KEY, ?TEST_OPTS]).

invalidate_url(Config) ->
    [PeerA, PeerB] = ?config(peers, Config),
    connect_peers(PeerA, PeerB),
    peer:call(PeerA#peer.peer,
              http_cache_store_disk,
              put,
              [?TEST_REQUEST_KEY,
               ?TEST_URL_DIGEST,
               ?TEST_VARY_HEADERS,
               ?TEST_RESPONSE,
               ?TEST_RESP_METADATA,
               ?TEST_OPTS]),
    timer:sleep(100),
    [_] =
        peer:call(PeerB#peer.peer,
                  http_cache_store_disk,
                  list_candidates,
                  [?TEST_REQUEST_KEY, ?TEST_OPTS]),
    peer:call(PeerA#peer.peer,
              http_cache_store_disk,
              invalidate_url,
              [?TEST_URL_DIGEST, ?TEST_OPTS]),
    timer:sleep(100),
    [] =
        peer:call(PeerB#peer.peer,
                  http_cache_store_disk,
                  list_candidates,
                  [?TEST_REQUEST_KEY, ?TEST_OPTS]).

invalidate_by_alternate_key(Config) ->
    [PeerA, PeerB] = ?config(peers, Config),
    connect_peers(PeerA, PeerB),
    peer:call(PeerA#peer.peer,
              http_cache_store_disk,
              put,
              [?TEST_REQUEST_KEY,
               ?TEST_URL_DIGEST,
               ?TEST_VARY_HEADERS,
               ?TEST_RESPONSE,
               ?TEST_RESP_METADATA,
               ?TEST_OPTS]),
    timer:sleep(100),
    [_] =
        peer:call(PeerB#peer.peer,
                  http_cache_store_disk,
                  list_candidates,
                  [?TEST_REQUEST_KEY, ?TEST_OPTS]),
    peer:call(PeerA#peer.peer,
              http_cache_store_disk,
              invalidate_by_alternate_key,
              [[alternate], ?TEST_OPTS]),
    timer:sleep(100),
    [] =
        peer:call(PeerB#peer.peer,
                  http_cache_store_disk,
                  list_candidates,
                  [?TEST_REQUEST_KEY, ?TEST_OPTS]).

configure_peer(PeerIndex, Config, TestName) ->
    PeerName = peer:random_name(atom_to_list(TestName) ++ integer_to_list(PeerIndex)),
    CacheDir = ?config(priv_dir, Config) ++ "/http_cache_tests/" ++ PeerName,
    {ok, Peer, Node} =
        ?CT_PEER(#{name => PeerName,
                   connection => standard_io,
                   args => ["-pa" | code:get_path()]}),
    peer:call(Peer,
              application,
              set_env,
              [[{http_cache_store_disk, [{cache_dir, CacheDir}, {cluster_enabled, true}]}]]),
    {ok, _} = peer:call(Peer, application, ensure_all_started, [http_cache_store_disk]),
    #peer{peer = Peer,
          node = Node,
          cache_dir = CacheDir}.

connect_peers(PeerA, PeerB) ->
    true = peer:call(PeerA#peer.peer, net_kernel, connect_node, [PeerB#peer.node]).
