-module(base_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([cache_file/1, lru_deletion_disk_limit/1, lru_deletion_memory_limit/1,
         invalidate_by_url/1, invalidate_by_alternate_key/1]).

-define(MEM_LIMIT_CHECK_INTERVAL, 100).
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

all() ->
    [cache_file,
     lru_deletion_disk_limit,
     lru_deletion_memory_limit,
     invalidate_by_url,
     invalidate_by_alternate_key].

init_per_testcase(TestName, Config) ->
    CacheDir = ?config(priv_dir, Config) ++ "/http_cache_tests/" ++ atom_to_list(TestName),
    application:set_env(http_cache_store_disk, cache_dir, CacheDir),
    application:set_env(http_cache_store_disk,
                        mem_limit_check_interval,
                        ?MEM_LIMIT_CHECK_INTERVAL),
    application:set_env(os_mon, disk_space_check_interval, {second, 1}),
    application:set_env(http_cache_store_disk, disk_limit, 1.0),
    application:set_env(http_cache_store_disk, memory_limit, 1.0),
    {ok, _} = application:ensure_all_started(http_cache_store_disk),
    [{cache_dir, CacheDir} | Config].

end_per_testcase(_, Config) ->
    ok = application:stop(http_cache_store_disk),
    ok = file:del_dir_r(?config(cache_dir, Config)).

cache_file(_Config) ->
    http_cache_store_disk:put(?TEST_REQUEST_KEY,
                              ?TEST_URL_DIGEST,
                              ?TEST_VARY_HEADERS,
                              ?TEST_RESPONSE,
                              ?TEST_RESP_METADATA,
                              ?TEST_OPTS),
    timer:sleep(1000),
    [{ObjectKey, _, _, _, _}] =
        http_cache_store_disk:list_candidates(?TEST_REQUEST_KEY, ?TEST_OPTS),
    {?TEST_STATUS, ?TEST_RESP_HEADERS, {file, FileName}, _} =
        http_cache_store_disk:get_response(ObjectKey, ?TEST_OPTS),
    {ok, ?TEST_RESP_BODY} == file:read_file(FileName).

lru_deletion_disk_limit(_Config) ->
    TelemetryRef =
        telemetry_test:attach_event_handlers(self(), [[http_cache_store_disk, object_deleted]]),
    http_cache_store_disk:put(?TEST_REQUEST_KEY,
                              ?TEST_URL_DIGEST,
                              ?TEST_VARY_HEADERS,
                              ?TEST_RESPONSE,
                              ?TEST_RESP_METADATA,
                              ?TEST_OPTS),
    application:set_env(http_cache_store_disk, disk_limit, 0.0),
    timer:sleep(disksup:get_check_interval() * 2),
    [] = http_cache_store_disk:list_candidates(?TEST_REQUEST_KEY, ?TEST_OPTS),
    receive
        {[http_cache_store_disk, object_deleted],
         TelemetryRef,
         #{},
         #{reason := lru_nuked_disk}} ->
            telemetry:detach(TelemetryRef)
    after 1000 ->
        ct:fail(timeout_receive_telemetry_event)
    end.

lru_deletion_memory_limit(_Config) ->
    TelemetryRef =
        telemetry_test:attach_event_handlers(self(), [[http_cache_store_disk, object_deleted]]),
    http_cache_store_disk:put(?TEST_REQUEST_KEY,
                              ?TEST_URL_DIGEST,
                              ?TEST_VARY_HEADERS,
                              ?TEST_RESPONSE,
                              ?TEST_RESP_METADATA,
                              ?TEST_OPTS),
    application:set_env(http_cache_store_disk, memory_limit, 0),
    timer:sleep(?MEM_LIMIT_CHECK_INTERVAL * 2),
    [] = http_cache_store_disk:list_candidates(?TEST_REQUEST_KEY, ?TEST_OPTS),
    receive
        {[http_cache_store_disk, object_deleted],
         TelemetryRef,
         #{},
         #{reason := lru_nuked_memory}} ->
            telemetry:detach(TelemetryRef)
    after 1000 ->
        ct:fail(timeout_receive_telemetry_event)
    end.

invalidate_by_url(_Config) ->
    TelemetryRef =
        telemetry_test:attach_event_handlers(self(), [[http_cache_store_disk, object_deleted]]),
    http_cache_store_disk:put(?TEST_REQUEST_KEY,
                              ?TEST_URL_DIGEST,
                              ?TEST_VARY_HEADERS,
                              ?TEST_RESPONSE,
                              ?TEST_RESP_METADATA,
                              ?TEST_OPTS),
    timer:sleep(1000),
    http_cache_store_disk:invalidate_url(?TEST_URL_DIGEST, ?TEST_OPTS),
    timer:sleep(1000),
    [] = http_cache_store_disk:list_candidates(?TEST_REQUEST_KEY, ?TEST_OPTS),
    receive
        {[http_cache_store_disk, object_deleted],
         TelemetryRef,
         #{},
         #{reason := url_invalidation}} ->
            telemetry:detach(TelemetryRef)
    after 1000 ->
        ct:fail(timeout_receive_telemetry_event)
    end.

invalidate_by_alternate_key(_Config) ->
    TelemetryRef =
        telemetry_test:attach_event_handlers(self(), [[http_cache_store_disk, object_deleted]]),
    http_cache_store_disk:put(?TEST_REQUEST_KEY,
                              ?TEST_URL_DIGEST,
                              ?TEST_VARY_HEADERS,
                              ?TEST_RESPONSE,
                              ?TEST_RESP_METADATA,
                              ?TEST_OPTS),
    timer:sleep(1000),
    http_cache_store_disk:invalidate_by_alternate_key([alternate], ?TEST_OPTS),
    timer:sleep(1000),
    [] = http_cache_store_disk:list_candidates(?TEST_REQUEST_KEY, ?TEST_OPTS),
    receive
        {[http_cache_store_disk, object_deleted],
         TelemetryRef,
         #{},
         #{reason := alternate_key_invalidation}} ->
            telemetry:detach(TelemetryRef)
    after 1000 ->
        ct:fail(timeout_receive_telemetry_event)
    end.
